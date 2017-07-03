-module(egambo_tcp_handler).

-include("egambo.hrl").

-behavior(etcpjson_srv).

% etcpjson_srv callbacks
-export([init/3, code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-record(state, {ip, port, srv}).

%% Callbacks
init(IP, Port, Srv) ->
    ?Info("peer connected ~s:~p", [inet:ntoa(IP), Port]),
    {ok, #state{ip = IP, port = Port, srv = Srv}}.

handle_info(Json, State) when is_map(Json) ->
    ?Info("Json data received: ~p", [Json]),
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
terminate(Reason, #state{ip = IP, port = Port}) ->
    ?Info("terminate ~s:~p : ~p", [inet:ntoa(IP), Port, Reason]).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% End Callbacks

-spec build_reply_fun(pid()) -> fun().
build_reply_fun(Self) ->
    fun(Reply) ->
        Self ! {reply, jsx:encode(Reply)}
    end.