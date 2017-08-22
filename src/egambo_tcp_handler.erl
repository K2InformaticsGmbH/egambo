-module(egambo_tcp_handler).

-include("egambo.hrl").

-behavior(etcpjson_srv).

% etcpjson_srv callbacks
-export([init/3, code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-record(state, {
    ip :: inet:ip_address(),
    port :: inet:port_number(),
    srv :: tuple(),
    session :: binary()
}).

%% Callbacks
init(IP, Port, Srv) ->
    ?Info("peer connected ~s:~p", [inet:ntoa(IP), Port]),
    SessionId = base64:encode(crypto:strong_rand_bytes(94)),
    MsgFun = build_reply_fun(msg, Srv),
    egambo_session_sup:start_session(SessionId, MsgFun),
    {ok, #state{ip = IP, port = Port, srv = Srv, session = SessionId}}.

handle_info(Json, #state{session = SessionId, srv = Srv} = State) when is_map(Json) ->
    ?Info("Json data received: ~p", [Json]),
    egambo_session:request(SessionId, Json, build_reply_fun(resp, Srv)),
    {noreply, State};
handle_info(Request, State) -> 
    ?Info("Unsolicited handle_info in ~p : ~p", [?MODULE, Request]),
    {noreply, State}.

handle_cast(Request, State) ->
    ?Info("Unsolicited handle_cast in ~p : ~p", [?MODULE, Request]),
    {noreply, State}.

handle_call(Request, From, State) ->
    {stop, {unsupported_call, Request, From}, unsupported, State}.

terminate({shutdown, Reason}, State) -> terminate(Reason, State);
terminate(Reason, #state{ip = IP, port = Port, session = SessionId}) ->
    ?Info("terminate ~s:~p : ~p", [inet:ntoa(IP), Port, Reason]),
    egambo_session_sup:close_session(SessionId).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% End Callbacks

-spec build_reply_fun(atom(), tuple()) -> fun().
build_reply_fun(Dir, Srv) ->
    fun(Msg) ->
        ?Info("~p ~p", [Dir, Msg]),
        Srv:send(#{Dir => Msg})
    end.