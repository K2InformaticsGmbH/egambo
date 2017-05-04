-module(egambo_resource).

-behaviour(cowboy_loop_handler).

-include("egambo.hrl").

-export([
    init/3,
    info/3,
    terminate/3
]).


%% Requests must be replied in 5 seconds for now
%% maybe too short but lets try to be responsive.
-define(REQUEST_TIMEOUT, 5000).

%% TODO: Add login & xsrf protection.
init({ssl, http}, Req, []) ->
    case cowboy_req:has_body(Req) of
        true ->
            {ok, Body, Req2} = cowboy_req:body(Req),
            ReplyFun = build_reply_fun(self()),
            egambo_session:request(imem_json:decode(Body, [return_maps]), ReplyFun),
            {loop, Req2, undefined, ?REQUEST_TIMEOUT, hibernate};
        _Else ->
            ?Error("request ~p doesn not contain body", [Req]),
            self() ! {reply, <<"{}">>},
            {loop, Req, undefined, 100}
    end.

info({reply, Body}, Req, State) when is_binary(Body) ->
    ?Debug("reply ~n~p to ~p", [Body, State]),
    {ok, Req2} = cowboy_req:reply(200, [
          {<<"content-encoding">>, <<"utf-8">>}
        , {<<"content-type">>, <<"application/json">>}
        ], Body, Req),
    {ok, Req2, State};
info(Message, Req, State) ->
    ?Error("~p unknown message in loop ~p", [self(), Message]),
    {loop, Req, State, hibernate}.

terminate(_Reason, _Req, _State) ->
    ok.

-spec build_reply_fun(pid()) -> fun().
build_reply_fun(Self) ->
    fun(Reply) ->
        Self ! {reply, jsx:encode(Reply)}
    end.
