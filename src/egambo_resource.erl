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
            XSRFToken = cowboy_req:header(?XSRF_HEADER, Req, <<>>),
            SessionToken = dderl:get_cookie(?COOKIE_NAME, Req, <<>>),
            ?Info("The xsrf token: ~p", [XSRFToken]),
            ?Info("The session token (cookie) ~p", [SessionToken]),
            SessionId = get_session(SessionToken),
            {ok, Body, Req2} = cowboy_req:read_body(Req), %% Might not be complete.
            ReplyFun = build_reply_fun(self()),
            egambo_session:request(SessionId, imem_json:decode(Body, [return_maps]), ReplyFun),
            {cowboy_loop, Req2, undefined};
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

-spec get_session(binary()) -> binary().
get_session(<<>>) ->
    SessionId = base64:encode(crypto:strong_rand_bytes(94)),
    egambo_session_sup:start_session(SessionId, build_notify_fun()),
    SessionId;
get_session(Token) ->
    Token.
