-module(egambo_game).

-include("egambo_game.hrl").

-behavior(gen_server).

-define(EG_MSG_TABLE_OPTS,  [{record_name, egGameMsg}
                            ,{type, ordered_set}
                            ,{purge_delay, 430000}      %% 430000 = 5 Days - 2000 sec
                            ]).

-define(NO_SUCH_GAME_TYPE, {error, <<"Game type does not exist">>}).
-define(NO_SUCH_GAME, {error, <<"Game does not exist">>}).
-define(UNIQUE_PLAYERS, {error, <<"You cannot play against yourself">>}).
-define(NOT_YOUR_GAME, {error, <<"This is not your game">>}).
-define(CANCEL_STATUS(__ST), {error, <<"You cannot cancel a game in status ", __ST/binary>>}).
-define(ACCEPT_MISMATCH, {error, <<"You cannot accept a challenge for another player">>}).
-define(ACCEPT_STATUS(__ST), {error, <<"You cannot accept a game in status ", __ST/binary>>}).
-define(BAD_COMMAND, {error, <<"Bad command or parameter format">>}).

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

-export([ create/2          % unconditionally create a game against anyone, do not try to match existing offerings
        , create/3          % challenge a specific player (accept a reciprocal challenge/matching game or create a new one)
        , start/2           % want to play against anyone (accept a forming game or create your own)
        , start/3           % want to challenge a specific player (accept a matching offering or create one)
        , cancel/2          % cancel a game in forming state, only possible before playing really starts
        , accept/2          % accept a specific challenge from someone
        , notify/4          % notify players and watchers about a game state change
        , result/1          % return map with current game result (board status, turn, scores)
        , moves/1           % return map with game history 
        , read_game/1       % return game status from egGame table
        , read_type/1       % return game status from egGame table
        , write_game/1      % write a game record to the db
        , eg_time/0         % return system timestamp in the format required for game tables
        ]).

-export([ play/4            % play one move: GameId, Cell|Command, Alias, AccountId     (not going through egambo_game gen_server if engine is running)
        , play/2            % play one move: GameId, Cell|Command                       (in the name of the next player)
        ]).

-safe([create, start, cancel, accept, play, result, moves, time]).

%% stateful game management functions (run through gen_server for serializability)

-spec create(egGameTypeId(), egAccountId()) -> egGameId() | egGameError().
create(GameType, MyAccountId) ->  gen_server:call(?MODULE, {create, GameType, MyAccountId}).

-spec create(egGameTypeId(), egAccountId(), egAccountId()) -> egGameId() | egGameError().
create(GameType, YourAccountId, MyAccountId) ->  gen_server:call(?MODULE, {create, GameType, YourAccountId, MyAccountId}).

-spec start(egGameTypeId(), egAccountId()) -> egGameId() | egGameError().
start(GameType, MyAccountId) ->  gen_server:call(?MODULE, {start, GameType, MyAccountId}).

-spec start(egGameTypeId(), egAccountId(), egAccountId()) -> egGameId() | egGameError().
start(GameType, YourAccountId, MyAccountId) ->  gen_server:call(?MODULE, {start, GameType, YourAccountId, MyAccountId}).

-spec cancel(egGameId(), egAccountId()) -> ok | egGameError().
cancel(GameId, MyAccountId) -> gen_server:call(?MODULE, {cancel, GameId, MyAccountId}).

-spec accept(egGameId(), egAccountId()) -> ok | egGameError().
accept(GameId, MyAccountId) -> gen_server:call(?MODULE, {accept, GameId, MyAccountId}).

-spec notify(egTime(), egGameId(), egGameMsgType(), egGameMsg()) -> ok | egGameError().
notify(EventTime, GameId, MessageType, Message) when is_tuple(EventTime), is_integer(GameId), is_atom(MessageType) -> 
    % ToDo: publish a message to be received by subscribed players and watchers
    imem_meta:write(egGameMsg, #egGameMsg{time=EventTime, gid=GameId, msgtype=MessageType, message=Message}).

%% stateless (db direct access) functions

-spec eg_time() -> egTime().
eg_time() -> imem_meta:time_uid().

-spec read_game(egGameId()) -> #egGame{} | egGameError().
read_game(GameId) -> 
    case catch imem_meta:read(egGame, GameId) of
        [#egGame{} = Game] ->   Game;
        _ ->                    ?NO_SUCH_GAME
    end.

-spec write_game(#egGame{}) -> ok | egGameError().
write_game(Game) ->  
    % ToDo: detect a finished game and update global score statsistics
    imem_meta:write(egGame, Game).

-spec read_type(egGameTypeId()) -> #egGameType{} | egGameError().
read_type(GameTypeId) -> 
    case catch imem_meta:read(egGameType, GameTypeId) of
        [#egGameType{} = Type] ->   Type;
        _ ->                        ?NO_SUCH_GAME_TYPE
    end.

-spec read_bot(egGameId()) -> #egGame{} | egGameError().
read_bot(AccountId) -> 
    case catch imem_meta:read(ddAccount, AccountId) of
        [Account] ->    
            case element(4, Account) of 
                user ->     undefined;
                deamon ->   undefined;
                Type -> 
                    case application:get_key(egambo, modules) of
                        {ok, Modules} ->
                            case lists:member(Type, Modules) of 
                                true ->     Type;
                                false ->    undefined
                            end;
                        _ -> 
                            undefined
                    end
            end;
        _ ->    
            undefined
    end.

-spec save_resume(#egGame{}) -> ok | egGameError().
save_resume(#egGame{gid=GameId, tid=GameType} = Game) ->
    write_game(Game), 
    case read_type(GameType) of
        #egGameType{engine=Engine} ->   Engine:resume(GameId);
        Error ->                        Error
    end.   

-spec resume(egGameId()) -> ok | egGameError().
resume(GameId) ->
    case read_game(GameId) of
        #egGame{tid=GameType} ->   
            case read_type(GameType) of
                #egGameType{engine=Engine} ->   Engine:resume(GameId);
                Error ->                        Error
            end;   
        Error ->
            Error
    end.  

-spec result(egGameId()) -> egGameResult() | egGameError().
result(GameId) -> 
    try 
        gen_server:call(?GLOBAL_ID(GameId), result)
    catch 
        exit:{noproc,_} ->             
            case read_game(GameId) of
                #egGame{tid=GameType} = Game ->   
                    case read_type(GameType) of
                        #egGameType{engine=Engine} ->   Engine:result(Game);
                        Error ->                        Error
                    end;   
                Error ->
                    Error
            end  
    end.

-spec moves(egGameId()) -> egGameMoves() | egGameError().
moves(GameId) -> 
    try 
        gen_server:call(?GLOBAL_ID(GameId), moves)
    catch
        exit:{noproc,_} ->             
            case read_game(GameId) of
                #egGame{tid=GameType} = Game ->   
                    case read_type(GameType) of
                        #egGameType{engine=Engine} ->   Engine:moves(Game);
                        Error ->                        Error
                    end;   
                Error ->
                    Error
            end  
    end.

%% stateless (engine access) functions with fallback to (stateful, serialized) engine creation

-spec play(egGameId(), egGameMove(), egAlias(), egAccountId()) -> ok | egGameError().
play(GameId, Move, MyAlias, MyAccountId) -> 
    engine_call(GameId, {play, Move, MyAlias, MyAccountId}).

-spec play(egGameId(), egGameMove()) -> ok | egGameError().
play(GameId, Move) -> 
    engine_call(GameId, {play, Move}).

engine_call(GameId, Command) -> 
    try 
        gen_server:call(?GLOBAL_ID(GameId), Command)
    catch
        exit:{noproc,_} ->        
            case resume(GameId) of 
                ok ->   
                    try 
                        gen_server:call(?GLOBAL_ID(GameId), Command)
                    catch 
                        _:Err -> Err
                    end; 
                Error ->    Error
            end
    end.    

% Internal helper functions
prepare(#egGame{tid=GameType} = Game) ->
    case read_type(GameType) of
        #egGameType{engine=Engine} = Type ->    Engine:prepare(Type, Game);
        _ ->                                    ?NO_SUCH_GAME_TYPE
    end.

init(_) ->
    Result = try
        imem_meta:init_create_table(egGameCategory, {record_info(fields, egGameCategory), ?egGameCategory, #egGameCategory{}}, [], system),  
        imem_meta:init_create_table(egGameType, {record_info(fields, egGameType), ?egGameType, #egGameType{}}, [], system),  
        imem_meta:init_create_table(egGame, {record_info(fields, egGame), ?egGame, #egGame{}}, [], system),  
        imem_meta:init_create_check_table(egGameMsg, {record_info(fields, egGameMsg), ?egGameMsg, #egGameMsg{}}, ?EG_MSG_TABLE_OPTS, system),
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

handle_call({create, GameType, MyAccountId}, _From, State) ->
    % Unconditionally create a game    
    case read_type(GameType) of
        #egGameType{cid=Cid} ->               
            GameId = rand:uniform(1844674407370955200),
            write_game(#egGame{ gid=GameId
                              , tid=GameType
                              , cid=Cid
                              , players=[MyAccountId]
                              , bots=[read_bot(MyAccountId)]
                              , ctime=eg_time()
                              }),
            {reply, GameId, State};
        _ -> 
            {reply, ?NO_SUCH_GAME_TYPE, State}
    end;
handle_call({create, _GameType, MyAccountId, MyAccountId}, _From, State) ->
        {reply, ?UNIQUE_PLAYERS, State};
handle_call({create, GameType, YourAccountId, MyAccountId}, _From, State) when is_binary(GameType), is_integer(YourAccountId), is_integer(MyAccountId) ->
    % Unconditionally create a challenge (game requesting a particular co-player)
    case imem_meta:read(egGameType, GameType) of
        [#egGameType{cid=Cid}] ->
            % ToDo: check for existing account               
            GameId = rand:uniform(1844674407370955200),
            write_game(#egGame{ gid=GameId
                              , tid=GameType
                              , cid=Cid
                              , players=[MyAccountId, YourAccountId]
                              , bots=[read_bot(YourAccountId), read_bot(MyAccountId)]
                              , ctime=eg_time()
                              }),
            {reply, GameId, State};
        _ -> 
            {reply, ?NO_SUCH_GAME_TYPE, State}
    end;
handle_call({start, GameType, MyAccountId}, From, State) when is_binary(GameType), is_integer(MyAccountId) ->
    % Find a matching challenge (requesting my participation) in forming state
    case imem_meta:select(egGame, [ {#egGame{tid=GameType, status=forming, players=['$1', MyAccountId], _ = '_'}
                                  , [{'/=', '$1', MyAccountId}]
                                  , ['$_']}
                                  ]) of        
        {[], true} ->           % forward to look for an any-player game in forming state
            handle_call({start_any, GameType, MyAccountId}, From, State);
        {Games, _} ->
            #egGame{gid=GameId} = Game = lists:nth(rand:uniform(length(Games)), Games),   % pick one game at random
            save_resume(prepare(Game#egGame{status=playing, stime=eg_time()})),
            {reply, GameId, State}                  
    end;
handle_call({start_any, GameType, MyAccountId}, From, State) when is_binary(GameType), is_integer(MyAccountId) ->    
    % Find a matching game (not requesting particular players) in forming state
    case imem_meta:select(egGame, [ {#egGame{tid=GameType, status=forming, players=['$1'], _ = '_'}
                                  , [{'/=', '$1', MyAccountId}]
                                  , ['$_']}
                                  ]) of        
        {[], true} ->           % no invitation exists, forward to unconditional game creation
            handle_call({create, GameType, MyAccountId}, From, State);
        {Games, _} ->
            #egGame{players=[P], bots=[B]} = Game = lists:nth(rand:uniform(length(Games)), Games),   % pick one game at random
            save_resume(prepare(Game#egGame{players=[P, MyAccountId], bots=[B, read_bot(MyAccountId)], status=playing, stime=eg_time()})),
            {reply, Game#egGame.gid, State}                  
    end;
handle_call({start, _GameType, MyAccountId, MyAccountId}, _From, State) ->
    {reply, ?UNIQUE_PLAYERS, State};
handle_call({start, GameType, YourAccountId, MyAccountId}, From, State) when is_binary(GameType), is_integer(YourAccountId), is_integer(MyAccountId) ->
    % Find a matching game offering (specific to me) create one
    case imem_meta:select(egGame, [ {#egGame{tid=GameType, status=forming, players=[YourAccountId, MyAccountId], _ = '_'}
                                  , []
                                  , ['$_']}
                                  ]) of        
        {[], true} ->
            handle_call({create, GameType, YourAccountId, MyAccountId}, From, State);
        {Games, _} ->
            Game = lists:nth(rand:uniform(length(Games)), Games),   % pick one game at random
            save_resume(prepare(Game#egGame{status=playing, stime=eg_time()})),
            {reply, Game#egGame.gid, State}                  
    end;
handle_call({cancel, GameId, MyAccountId}, _From, State) when is_integer(GameId), is_integer(MyAccountId) ->
    case imem_meta:read(egGame, GameId) of
        [#egGame{status=forming, players=[MyAccountId]}] ->               
            imem_meta:delete(egGame, GameId),
            {reply, ok, State};
        [#egGame{status=forming, players=[MyAccountId,_]}] ->               
            imem_meta:delete(egGame, GameId),
            {reply, ok, State};
        [#egGame{status=forming}] ->               
            {reply, ?NOT_YOUR_GAME, State};
        [#egGame{status=Status}] ->
            S = atom_to_binary(Status, utf8),
            {reply, ?CANCEL_STATUS(S), State};
        _ -> 
            {reply, ?NO_SUCH_GAME, State}
    end;
handle_call({accept, GameId, MyAccountId}, _From, State) when is_integer(GameId), is_integer(MyAccountId) ->
    case imem_meta:read(egGame, GameId) of
        [#egGame{status=forming, players=[_, MyAccountId]} = Game] ->
            save_resume(prepare(Game#egGame{status=playing, stime=eg_time()})), 
            {reply, ok, State};
        [#egGame{status=forming, players=[MyAccountId, _]}] ->               
            {reply, ?UNIQUE_PLAYERS, State};
        [#egGame{status=forming, players=[MyAccountId]}] ->               
            {reply, ?UNIQUE_PLAYERS, State};
        [#egGame{status=forming, players=[Player], bots=[Bot]} = Game] ->
            save_resume(prepare(Game#egGame{ status=playing
                                                 , players=[Player, MyAccountId]
                                                 , bots=[Bot, read_bot(MyAccountId)]
                                                 , stime=eg_time()
                                                 }
                                     )),               
            {reply, ok, State};
        [#egGame{status=forming, players=[_, _]}] ->               
            {reply, ?ACCEPT_MISMATCH, State};
        [#egGame{status=Status}] ->
            S = atom_to_binary(Status, utf8),
            {reply, ?ACCEPT_STATUS(S), State};
        _ -> 
            {reply, ?NO_SUCH_GAME, State}
    end;
handle_call(_Request, _From, State) -> {reply, ?BAD_COMMAND, State}.

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


% egambo_game:create(<<"tic_tac_toe">>, 2).
% egambo_game:create(<<"tic_tac_toe">>, 1, 2).
% egambo_game:start(<<"tic_tac_toe">>,  2).
% egambo_game:start(<<"tic_tac_toe">>, 1,  2).
% egambo_game:cancel(926946506377236097, 2).
% egambo_game:accept(1227778950635753473, 2).
% egambo_game:accept(1227778950635753473, 1).
% egambo_game:status(GameId, 2).
% egambo_game:play(664820677776998785, 7, $X, 2).
% egambo_game:result(664820677776998785).
% egambo_game:moves(664820677776998785).
% egambo_tictac:resume(72673005093445425).
% egambo_tictac:play(72673005093445425, 5, $X, 2).
% egambo_game:play(72673005093445425,a1).
