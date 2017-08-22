-module(egambo_player).
-behaviour(gen_server).

-include("egambo.hrl").

-export([
    start_link/1,
    register/2,
    notify/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    playerId :: egAccountId(),
    sessions :: map()
}).

-spec start_link(egAccountId()) -> {ok, pid()} | {error, term()}.
start_link(PlayerId) ->
    gen_server:start_link(?PLAYER_GID(PlayerId), ?MODULE, [PlayerId], []).

-spec register(egAccountId(), fun()) -> ok | {error, binary()}.
register(PlayerId, NotifyFun) ->
    try gen_server:call(?PLAYER_GID(PlayerId), {register, NotifyFun}) of
        ok -> ok
    catch
        exit:{noproc, _} ->
            case egambo_player_sup:start_player(PlayerId) of
                {ok, _Pid} ->
                    gen_server:call(?PLAYER_GID(PlayerId), {register, NotifyFun});
                {error, {already_started, _Pid}} ->
                    gen_server:call(?PLAYER_GID(PlayerId), {register, NotifyFun});
                Error ->
                    ?Error("Unable to start gen_server for player ~p error ~p", [PlayerId, Error]),
                    {error, <<"Unable to start player process">>}
            end
    end.

-spec notify(egAccountId(), map()) -> ok.
notify(PlayerId, Msg) ->
    gen_server:cast(?PLAYER_GID(PlayerId), {notify, Msg}).

init([PlayerId]) ->
    {ok, #state{playerId = PlayerId, sessions = #{}}}.

handle_call({register, NotifyFun}, {Pid, _Ref}, #state{sessions = Sessions} = State) ->
    erlang:monitor(process, Pid),
    {reply, ok, State#state{sessions = Sessions#{Pid => NotifyFun} }};
handle_call(Req, _From, #state{playerId = PlayerId} = State) ->
    ?Error("Invalid call request ~p received by player ~p process ~p", [PlayerId, Req, self()]),
    {reply, {error, <<"invalid request">>}, State}.

handle_cast({notify, Msg}, #state{sessions = Sessions} = State) ->
    [NotifyFun(Msg) || NotifyFun <- maps:values(Sessions)],
    {noreply, State};
handle_cast(Request, #state{playerId = PlayerId} = State) ->
    ?Error("Unsolicited cast ~p for player ~p", [Request, PlayerId]),
    {noreply, State}.

handle_info({'DOWN', _, process, Pid, _}, #state{sessions = Sessions} = State ) ->
    {noreply, State#state{sessions = maps:remove(Pid, Sessions)}};
handle_info(Info, #state{playerId = PlayerId} = State) ->
    ?Error("Unsolicited message ~p for player ~p", [Info, PlayerId]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.