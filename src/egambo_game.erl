-module(egambo_game).

-include("egambo_game.hrl").

-behavior(gen_server).

-record(state, {}).

-export([ start_link/0
        ]).

% gen_server behavior callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        , format_status/2
        ]).

-export([ create/3          % unconditionally create a game against anyone, do not try to match existing offerings
        , create/4          % challenge a specific player (accept a reciprocal challenge/matching game or create a new one)
        , start/3           % want to play against anyone (accept a forming game or create your own)
        , start/4           % want to challenge a specific player (accept a matching offering or create one)
        , cancel/3          % cancel a game in forming state, only possible before playing really starts
        , accept/3          % accept a specific challenge from someone
        , status/3          % read (current) game status
        ]).

-export([ play/4            % play one move in a game (not going through egambo_game gen_server)
        ]).

-safe([create, start, cancel, play, status]).


-spec create(integer(), ddOptions(), ddEntityId()) -> egGameId() | egGameError().
create(GameType, Opts, MyAccountId) ->  gen_server:call(?MODULE, {create, GameType, Opts, MyAccountId}).

-spec create(integer(), ddEntityId(), ddOptions(), ddEntityId()) -> egGameId() | egGameError().
create(GameType, YourAccountId, Opts, MyAccountId) ->  gen_server:call(?MODULE, {create, GameType, YourAccountId, Opts, MyAccountId}).

-spec start(integer(), ddOptions(), ddEntityId()) -> egGameId() | egGameError().
start(GameType, Opts, MyAccountId) ->  gen_server:call(?MODULE, {start, GameType, Opts, MyAccountId}).

-spec start(integer(), ddEntityId(), ddOptions(), ddEntityId()) -> egGameId() | egGameError().
start(GameType, YourAccountId, Opts, MyAccountId) ->  gen_server:call(?MODULE, {start, GameType, YourAccountId, Opts, MyAccountId}).

-spec cancel(egGameId(), ddOptions(), ddEntityId()) -> ok | egGameError().
cancel(GameId, Opts, MyAccountId) -> gen_server:call(?MODULE, {cancel, GameId, Opts, MyAccountId}).

-spec accept(egGameId(), ddOptions(), ddEntityId()) -> ok | egGameError().
accept(GameId, Opts, MyAccountId) -> gen_server:call(?MODULE, {accept, GameId, Opts, MyAccountId}).

-spec play(egGameId(), egGameMove(), ddOptions(), ddEntityId()) -> ok | egGameError().
play(_GameId, _Move, _Opts, _MyAccountId) -> ?egGameNotImplemented.

-spec status(egGameId(), ddOptions(), ddEntityId()) -> egGameResult() | egGameError().
status(GameId, Opts, MyAccountId) -> gen_server:call(?MODULE, {status, GameId, Opts, MyAccountId}).

init(_) ->
    Result = try
        imem_meta:init_create_table(egGameCategory, {record_info(fields, egGameCategory), ?egGameCategory, #egGameCategory{}}, [], system),  
        imem_meta:init_create_table(egGameType, {record_info(fields, egGameType), ?egGameType, #egGameType{}}, [], system),  
        imem_meta:init_create_table(egGame, {record_info(fields, egGame), ?egGame, #egGame{}}, [], system),  
        process_flag(trap_exit, true),
        {ok,#state{}}
    catch
        _Class:Reason -> {stop, {Reason,erlang:get_stacktrace()}} 
    end,
    Result.

start_link() ->
    ?Info("~p starting...~n", [?MODULE]),
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], [{spawn_opt, [{fullsweep_after, 0}]}]) of
        {ok, _} = Success ->
            ?Info("~p started!~n", [?MODULE]),
            Success;
        Error ->
            ?Error("~p failed to start ~p~n", [?MODULE, Error]),
            Error
    end.


handle_call({create, GameType, _Opts, MyAccountId}, _From, State) ->
    % Unconditionally create a game
    case imem_meta:read(egGameType, GameType) of
        [#egGameType{cid=Cid}] ->               
            GameId = rand:uniform(1844674407370955200),
            imem_meta:write(egGame, #egGame{gid=GameId, tid=GameType, cid=Cid, players=[MyAccountId], ctime=imem_meta:time_uid()}),
            {reply, GameId, State};
        _ -> 
            {reply, {error, <<"Unknown game type">>}, State}
    end;
handle_call({create, _GameType, MyAccountId, _Opts, MyAccountId}, _From, State) ->
        {reply, {error, <<"You cannot create a game with yourself">>}, State};
handle_call({create, GameType, YourAccountId, _Opts, MyAccountId}, _From, State) ->
    % Unconditionally create a challenge
    case imem_meta:read(egGameType, GameType) of
        [#egGameType{cid=Cid}] ->
            % ToDo: check for existing account               
            GameId = rand:uniform(1844674407370955200),
            imem_meta:write(egGame, #egGame{gid=GameId, tid=GameType, cid=Cid, players=[MyAccountId, YourAccountId], ctime=imem_meta:time_uid()}),
            {reply, GameId, State};
        _ -> 
            {reply, {error, <<"Unknown game type">>}, State}
    end;
handle_call({start, GameType, Opts, MyAccountId}, From, State) ->
    % Find a matching game offering (specific to me) or forward to look for an any-match below
    case imem_meta:select(egGame, [ {#egGame{tid=GameType, status=forming, players=['$1', MyAccountId], _ = '_'}
                                  , [{'/=', '$1', MyAccountId}]
                                  , ['$_']}
                                  ]) of        
        {[], true} ->
            handle_call({start_any, GameType, Opts, MyAccountId}, From, State);
        {[Game], true} ->
            imem_meta:write(egGame, Game#egGame{status=playing, stime=imem_meta:time_uid()}),
            % ToDo: spawn game serving instance
            {reply, Game#egGame.gid, State};
        {Games, _} ->
            Game = lists:nth(rand:uniform(length(Games)), Games),   % pick one game at random
            imem_meta:write(egGame, Game#egGame{status=playing, stime=imem_meta:time_uid()}),
            % ToDo: spawn game serving instance
            {reply, Game#egGame.gid, State}                  
    end;
handle_call({start_any, GameType, Opts, MyAccountId}, From, State) ->    
    case imem_meta:select(egGame, [ {#egGame{tid=GameType, status=forming, players=['$1'], _ = '_'}
                                  , [{'/=', '$1', MyAccountId}]
                                  , ['$_']}
                                  ]) of        
        {[], true} ->
            handle_call({create, GameType, Opts, MyAccountId}, From, State);
        {[Game], true} ->
            Players = Game#egGame.players ++ [MyAccountId],         % append myself to the player list
            imem_meta:write(egGame, Game#egGame{players=Players, status=playing, stime=imem_meta:time_uid()}),
            % ToDo: spawn game serving instance
            {reply, Game#egGame.gid, State};
        {Games, _} ->
            Game = lists:nth(rand:uniform(length(Games)), Games),   % pick one game at random
            Players = Game#egGame.players ++ [MyAccountId],         % append myself to the player list
            imem_meta:write(egGame, Game#egGame{players=Players, status=playing, stime=imem_meta:time_uid()}),
            % ToDo: spawn game serving instance
            {reply, Game#egGame.gid, State}                  
    end;
handle_call({start, _GameType, MyAccountId, _Opts, MyAccountId}, _From, State) ->
    {reply, {error, <<"You cannot start a game with yourself">>}, State};
handle_call({start, GameType, YourAccountId, Opts, MyAccountId}, From, State) ->
    % Find a matching game offering (specific to me) create one
    case imem_meta:select(egGame, [ {#egGame{tid=GameType, status=forming, players=[YourAccountId, MyAccountId], _ = '_'}
                                  , []
                                  , ['$_']}
                                  ]) of        
        {[], true} ->
            handle_call({create, GameType, YourAccountId, Opts, MyAccountId}, From, State);
        {[Game], true} ->
            imem_meta:write(egGame, Game#egGame{status=playing, stime=imem_meta:time_uid()}),
            % ToDo: spawn game serving instance
            {reply, Game#egGame.gid, State};
        {Games, _} ->
            Game = lists:nth(rand:uniform(length(Games)), Games),   % pick one game at random
            imem_meta:write(egGame, Game#egGame{status=playing, stime=imem_meta:time_uid()}),
            % ToDo: spawn game serving instance
            {reply, Game#egGame.gid, State}                  
    end;
handle_call({cancel, GameId, _Opts, MyAccountId}, _From, State) ->
    case imem_meta:read(egGame, GameId) of
        [#egGame{status=forming, players=[MyAccountId]}] ->               
            imem_meta:delete(egGame, GameId),
            {reply, ok, State};
        [#egGame{status=forming, players=[MyAccountId,_]}] ->               
            imem_meta:delete(egGame, GameId),
            {reply, ok, State};
        [#egGame{status=forming}] ->               
            {reply, {error, <<"This is not your game">>}, State};
        [#egGame{status=Status}] ->
            S = atom_to_binary(Status, utf8),
            {reply, {error, <<"You cannot cancel a game in status ", S/binary>>}, State};
        _ -> 
            {reply, {error, <<"Game does not exist">>}, State}
    end;
handle_call({accept, GameId, _Opts, MyAccountId}, _From, State) ->
    case imem_meta:read(egGame, GameId) of
        [#egGame{status=forming, players=[_, MyAccountId]} = Game] ->               
            imem_meta:write(egGame, Game#egGame{status=playing, stime=imem_meta:time_uid()}),
            % ToDo: spawn game serving instance
            {reply, ok, State};
        [#egGame{status=forming, players=[MyAccountId, _]}] ->               
            {reply, {error, <<"You cannot accept a game with yourself">>}, State};
        [#egGame{status=forming, players=[MyAccountId]}] ->               
            {reply, {error, <<"You cannot accept a game with yourself">>}, State};
        [#egGame{status=forming, players=[Player]} = Game] ->               
            imem_meta:write(egGame, Game#egGame{status=playing, players=[Player, MyAccountId], stime=imem_meta:time_uid()}),
            % ToDo: spawn game serving instance
            {reply, ok, State};
        [#egGame{status=forming, players=[_, _]}] ->               
            {reply, {error, <<"You cannot accept a challenge for another player">>}, State};
        [#egGame{status=Status}] ->
            S = atom_to_binary(Status, utf8),
            {reply, {error, <<"You cannot accept a game in status ", S/binary>>}, State};
        _ -> 
            {reply, {error, <<"Game does not exist">>}, State}
    end;
handle_call({status, GameId, _Opts, _MyAccountId}, _From, State) ->
    case imem_meta:read(egGame, GameId) of
        [#egGame{} = Game] ->               
            {reply, Game, State};
        _ -> 
            {reply, {error, <<"Game does not exist">>}, State}
    end;
handle_call(_Request, _From, State) -> {reply, ?egGameNotImplemented, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(normal, _State) -> ?Info("~p normal stop~n", [?MODULE]);
terminate(shutdown, _State) -> ?Info("~p shutdown~n", [?MODULE]);
terminate({shutdown, _Term}, _State) -> ?Info("~p shutdown : ~p~n", [?MODULE, _Term]);
terminate(Reason, _State) -> ?Error("~p stopping unexpectedly : ~p~n", [?MODULE, Reason]).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

format_status(_Opt, [_PDict, _State]) -> ok.


