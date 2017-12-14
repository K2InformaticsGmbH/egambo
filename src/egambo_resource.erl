-module(egambo_resource).

-behaviour(cowboy_loop).

-include("egambo.hrl").

-export([
    init/2,
    info/3,
    terminate/3
]).


%% Requests must be replied in 5 seconds for now
%% maybe too short but lets try to be responsive.
-define(REQUEST_TIMEOUT, 5000).

-define(COOKIE_NAME, <<"EGAMBO-SESSION">>).

%% TODO: Add login & xsrf protection.
init(Req, []) ->
    case cowboy_req:has_body(Req) of
        true ->
            SessionToken = dderl:get_cookie(?COOKIE_NAME, Req, <<>>),
            XSRFToken = cowboy_req:header(?XSRF_HEADER, Req, <<>>),
            ?Info("The xsrf token: ~p", [XSRFToken]),
            ?Info("The session token (cookie) ~p", [SessionToken]),
            {ok, Body, Req2} = cowboy_req:read_body(Req), %% Might not be complete.
            ReplyFun = build_reply_fun(self()),
            [Action] = cowboy_req:path_info(Req),
            Args = imem_json:decode(Body, [return_maps]),
            ?Info("Requested action ~p and args ~p", [Action, Args]),
            case get_session(SessionToken, XSRFToken) of
                {error, xsrf} ->
                    {ok, Req2, undefined};
                {SessionId, NewXSRFToken, IsNewSession} ->
                    Req3 = case IsNewSession of
                        true -> set_cookies(Req2, SessionId, NewXSRFToken);
                        false -> Req2
                    end,
                    egambo_session:request(SessionId, Args#{<<"action">> => Action}, ReplyFun),
                    {cowboy_loop, Req3, undefined}
            end;
        _Else ->
            ?Error("request ~p doesn not contain body", [Req]),
            self() ! {reply, <<"{}">>},
            {cowboy_loop, Req, undefined}
    end.

info({reply, Body}, Req, State) when is_binary(Body) ->
    ?Debug("reply ~n~p to ~p", [Body, State]),
    Req2 = cowboy_req:reply(200, #{
        <<"content-encoding">> => <<"utf-8">>,
        <<"content-type">> => <<"application/json">>
    }, Body, Req),
    {ok, Req2, State};
info(Message, Req, State) ->
    ?Error("~p unknown message in loop ~p", [self(), Message]),
    {cowboy_loop, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.

-spec build_reply_fun(pid()) -> fun().
build_reply_fun(Self) ->
    fun(Reply) ->
        Self ! {reply, jsx:encode(#{resp => Reply})}
    end.

-spec build_notify_fun() -> fun().
build_notify_fun() ->
    fun(Msg) ->
        %% TODO: This should be websockets or long polling...
        ?Info("Notification ~p", [Msg]),
        ok
    end.

-spec get_session(binary(), term()) -> {binary(), binary(), boolean()} | {error, xsrf}.
get_session(Token, XSRFToken) ->
    case global:whereis_name(Token) of
        undefined ->
            SessionId = base64:encode(crypto:strong_rand_bytes(94)),
            NewXSRFToken = base64:encode(crypto:strong_rand_bytes(32)),
            Result = egambo_session_sup:start_session(SessionId, NewXSRFToken, build_notify_fun()),
            ?Info("The result ~p", [Result]),
            {SessionId, NewXSRFToken, true};
        _SessionPid ->
            case egambo_session:get_xsrfToken(Token) of
                XSRFToken ->
                    {Token, XSRFToken, false};
                Expected ->
                    ?Error("XSRF attack expected : ~p got : ~p", [Expected, XSRFToken]),
                    {error, xsrf}
            end
    end.

set_cookies(Req, <<>>, <<>>) ->
    set_cookies(Req, <<>>, <<>>, #{max_age => 0}); %% to delete the cookie 
set_cookies(Req, SessionToken, XSRFToken) ->
    set_cookies(Req, SessionToken, XSRFToken, #{}).

set_cookies(Req, SessionToken, XSRFToken, Opts) ->
    Host = cowboy_req:host(Req),
    %% We are in the root.
    Path = dderl:format_path(""),
    Req1 = cowboy_req:set_resp_cookie(?COOKIE_NAME, SessionToken, Req,
                                      maps:merge(Opts, ?HTTP_ONLY_COOKIE_OPTS(Host, Path))),
    cowboy_req:set_resp_cookie(?XSRF_COOKIE, XSRFToken, Req1,
                               maps:merge(Opts, ?COOKIE_OPTS(Host, Path))).
