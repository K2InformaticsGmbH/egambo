-module(egambo_game).

-include("egambo_game.hrl").

-behavior(gen_server).

-define(EG_MSG_TABLE_OPTS,  [{record_name, egGameMsg}
                            ,{type, ordered_set}
                            ,{purge_delay, 430000}      %% 430000 = 5 Days - 2000 sec
                            ]).

-define(MAX_BATCH_COUNT, 10000).

-define(BAD_BATCH_START_REQUEST, {error, <<"Bad batch start command or batch count exceeded">>}).
-define(NO_SUCH_GAME_TYPE, {error, <<"Game type does not exist">>}).
-define(NO_SUCH_GAME, {error, <<"Game does not exist">>}).
-define(UNIQUE_PLAYERS, {error, <<"You cannot play against yourself">>}).
-define(NOT_YOUR_GAME, {error, <<"This is not your game">>}).
-define(CANCEL_STATUS(__ST), {error, <<"You cannot cancel a game in status ", __ST/binary>>}).
-define(ACCEPT_MISMATCH, {error, <<"You cannot accept a challenge for another player">>}).
-define(ACCEPT_STATUS(__ST), {error, <<"You cannot accept a game in status ", __ST/binary>>}).
-define(BAD_COMMAND, {error, <<"Bad command or parameter format">>}).
-define(BAD_ACCOUNT, {error, <<"Bad account format">>}).

-define(DEFAULT_GAME_CATEGORIES, [ {egGameCategory,<<"tictac_challenge">>,<<"Tic Tac Challenge">>,<<"Two players alternate in placing their stones on a square board. A minimum number of consecutive stones in horizontal, vertical or diagonal direction wins. ">>}
                                 ]).
-define(DEFAULT_GAME_TYPES, [
  {egGameType,<<"tic_tac_toe_p">>,<<"3x3:3 Tic-Tac-Toe periodic">>,<<"tictac_challenge">>,egambo_tictac,2,#{aliases => "XO",gravity => false,height => 3,jokers => 0,obstacles => 0,periodic => true,run => 3,width => 3},undefined,1,<<"Tic-Tac-Toe on a periodic Board">>}
, {egGameType,<<"tic_tac_toe_g">>,<<"3x3:3 Tic-Tac-Toe with gravity">>,<<"tictac_challenge">>,egambo_tictac,2,#{aliases => "XO",gravity => true,height => 3,jokers => 0,obstacles => 0,periodic => false,run => 3,width => 3},undefined,1,<<"Tic-Tac-Toe with Gravity ">>}
, {egGameType,<<"gomoku_8">>,<<"8x8:5 Gomoku small ">>,<<"tictac_challenge">>,egambo_tictac,2,#{aliases => "XO",gravity => false,height => 8,jokers => 0,obstacles => 0,periodic => false,run => 5,width => 8},undefined,2,<<"Small Board Gomoku on 8x8 ">>}
, {egGameType,<<"tic_tac_toe">>,<<"3x3:3 Tic-Tac-Toe classic">>,<<"tictac_challenge">>,egambo_tictac,2,#{aliases => "XO",gravity => false,height => 3,jokers => 0,obstacles => 0,periodic => false,run => 3,width => 3},undefined,1,<<"Tic-Tac-Toe classic ">>}
, {egGameType,<<"connect_four">>,<<"7x6:4g Connect Four classic">>,<<"tictac_challenge">>,egambo_tictac,2,#{aliases => "XO",gravity => true,height => 6,jokers => 0,obstacles => 0,periodic => false,run => 4,width => 7},undefined,2,<<"Classic Connect Four (also Captain's mistress) ">>}
, {egGameType,<<"tic_tac_toe_443_oj">>,<<"4x4:3 Tic-Tac-Toe 1 obstacle 1 joker">>,<<"tictac_challenge">>,egambo_tictac,2,#{aliases => "XO",gravity => false,height => 4,jokers => 1,obstacles => 1,periodic => false,run => 3,width => 4},undefined,1,<<"Tic-Tac-Toe 4x4:3 with obstacle and joker">>}
]).

-define(DEFAULT_ACCOUNTS, [
  {ddAccount,1,<<"bot1">>,egambo_tictac_bot,[{scrypt,{<<58,37,63,119,99,72,15,176,129,198,51,111,57,75,39,108,115,32,240,124,125,78,88,192,158,90,201,226,44,187,133,119>>,<<114,104,245,152,103,199,134,8,178,201,226,224,8,20,249,200,124,66,148,136,249,59,245,209,138,158,181,66,138,205,7,7,106,136,30,150,216,105,38,32,87,74,240,42,101,1,150,56,78,96,67,9,215,136,94,165,226,231,127,237,80,158,141,231>>}}],<<"Bot1 simple tic_tac_bot">>,undefined,undefined,{{2017,3,26},{20,54,40}},false}
, {ddAccount,2,<<"bot2">>,egambo_tictac_bot,[{scrypt,{<<58,37,63,119,99,72,15,176,129,198,51,111,57,75,39,108,115,32,240,124,125,78,88,192,158,90,201,226,44,187,133,119>>,<<114,104,245,152,103,199,134,8,178,201,226,224,8,20,249,200,124,66,148,136,249,59,245,209,138,158,181,66,138,205,7,7,106,136,30,150,216,105,38,32,87,74,240,42,101,1,150,56,78,96,67,9,215,136,94,165,226,231,127,237,80,158,141,231>>}}],<<"Bot2 simple tic_tac_bot">>,undefined,undefined,{{2017,3,26},{20,54,40}},false}
, {ddAccount,3,<<"player3">>,user,[{scrypt,{<<58,37,63,119,99,72,15,176,129,198,51,111,57,75,39,108,115,32,240,124,125,78,88,192,158,90,201,226,44,187,133,119>>,<<114,104,245,152,103,199,134,8,178,201,226,224,8,20,249,200,124,66,148,136,249,59,245,209,138,158,181,66,138,205,7,7,106,136,30,150,216,105,38,32,87,74,240,42,101,1,150,56,78,96,67,9,215,136,94,165,226,231,127,237,80,158,141,231>>}}],<<"Player3">>,undefined,undefined,{{2017,3,26},{20,54,40}},false}
, {ddAccount,4,<<"player4">>,user,[{scrypt,{<<58,37,63,119,99,72,15,176,129,198,51,111,57,75,39,108,115,32,240,124,125,78,88,192,158,90,201,226,44,187,133,119>>,<<114,104,245,152,103,199,134,8,178,201,226,224,8,20,249,200,124,66,148,136,249,59,245,209,138,158,181,66,138,205,7,7,106,136,30,150,216,105,38,32,87,74,240,42,101,1,150,56,78,96,67,9,215,136,94,165,226,231,127,237,80,158,141,231>>}}],<<"Player4">>,undefined,undefined,{{2017,3,26},{20,54,40}},false}
]).

-define(DEFAULT_ROLES, [
  {ddRole,system,[egambo,dderl],[manage_system,manage_accounts,manage_system_tables,manage_user_tables,{dderl,con,local,use}],[]}
, {ddRole,egambo,[],[{eval_mfa,egambo_game,create}
                    ,{eval_mfa,egambo_game,start}
                    ,{eval_mfa,egambo_game,cancel}
                    ,{eval_mfa,egambo_game,play}
                    ,{eval_mfa,egambo_game,result}
                    ,{eval_mfa,egambo_game,moves}
                    ,{eval_mfa,egambo_game,sample}
                    ],[]}
, {ddRole,dderl,[],[{dderl,restart},{dderl,conn,local,use},{dderl,conn,{owner,system},use},{dderl,conn,manage}],[]}
, {ddRole,1,[],[],[]}
, {ddRole,2,[],[],[]}
, {ddRole,3,[system],[],[]}
, {ddRole,4,[system],[],[]}
]).

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
        , create/4          % create multiple challenges in a batch
        , start/2           % want to play against anyone (accept a forming game or create your own)
        , start/3           % want to challenge a specific player (accept a matching offering or create one)
        , cancel/2          % cancel a game in forming state, only possible before playing really starts
        , accept/2          % accept a specific challenge from someone
        , notify/5          % notify players and watchers about a game state change
        , result/1          % return map with current game result (board status, turn, scores)
        , moves/1           % return map with game history 
        , read_game/1       % return game status from egGame table
        , read_type/1       % return game status from egGame table
        , write_game/1      % write a game record to the db
        , resume/1          % resume (restart from saved state) one or several games
        , stop/1            % stop (forward stop stignal to engine) one or more games
        , resume_bots/2     % check if bots are running, resume them if necessary
        , eg_time/0         % return system timestamp in the format required for game tables
        , eg_time_to_sec/1
        , eg_time_to_msec/1
        , eg_time_to_usec/1
        ]).

-export([ play/4            % play one move: GameId, Cell|Command, Alias, AccountId     (not going through egambo_game gen_server if engine is running)
        , play/2            % play one move: GameId, Cell|Command                       (in the name of the next player)
        , play_bot/5        % trigger a bot to play one move (async)
        ]).

-safe([create, start, cancel, accept, play, result, moves, time, sample]).

%% stateful game management functions (run through gen_server for serializability)

-spec create(egGameTypeId(), egAccountId()) -> egGameId() | egGameError().
create(GameType, MyAcc) when is_integer(MyAcc) ->  
    gen_server:call(?MODULE, {create, GameType, MyAcc});
create(_, _) -> ?BAD_ACCOUNT.

-spec create(egGameTypeId(), egAccountId(), egAccountId()) -> egGameId() | egGameError().
create(GameType, YourAcc, MyAcc) when is_integer(MyAcc), is_integer(YourAcc) ->  
    gen_server:call(?MODULE, {create, GameType, YourAcc, MyAcc});
create(_, _, _) -> ?BAD_ACCOUNT.

-spec create(egGameTypeId(), integer(), egAccountId(), egAccountId()) -> [egGameId() | egGameError()] | egGameError().
create(GameType, Cnt, YourAcc, MyAcc) when is_integer(MyAcc), is_integer(YourAcc), is_integer(Cnt), Cnt > 0, Cnt =< ?MAX_BATCH_COUNT ->  
    [gen_server:call(?MODULE, {create, GameType, YourAcc, MyAcc}) || _ <- lists:seq(1, Cnt)];
create(_, _, YourAcc, MyAcc) when is_integer(MyAcc), is_integer(YourAcc) ->  ?BAD_BATCH_START_REQUEST;
create(_, _, _, _) -> ?BAD_ACCOUNT.

-spec start(egGameTypeId(), egAccountId()) -> egGameId() | egGameError().
start(GameType, MyAcc) when is_integer(MyAcc) ->  
    gen_server:call(?MODULE, {start, GameType, MyAcc});
start(_, _) -> ?BAD_ACCOUNT.

-spec start(egGameTypeId(), egAccountId(), egAccountId()) -> egGameId() | egGameError().
start(GameType, YourAcc, MyAcc) when is_integer(MyAcc), is_integer(YourAcc) ->  
    gen_server:call(?MODULE, {start, GameType, YourAcc, MyAcc});
start(_, _, _) -> ?BAD_ACCOUNT.

-spec cancel(egGameId(), egAccountId()) -> ok | egGameError().
cancel(GameId, MyAcc) when is_integer(MyAcc) -> 
    gen_server:call(?MODULE, {cancel, GameId, MyAcc});
cancel(_, _) -> ?BAD_ACCOUNT.

-spec accept(egGameId(), egAccountId()) -> ok | egGameError().
accept(GameId, MyAcc) when is_integer(MyAcc) -> 
    gen_server:call(?MODULE, {accept, GameId, MyAcc});
accept(_, _) -> ?BAD_ACCOUNT.

-spec notify(egTime(), egGameId(), egGameMsgType(), egGameMsg(), [egBotId()]) -> ok | egGameError().
notify(EventTime, GameId, MessageType, Message, Bots) when is_tuple(EventTime), is_integer(GameId), is_atom(MessageType) -> 
    case lists:member(undefined, Bots) of
        true ->
            % ToDo: publish a message to be received by subscribed players and watchers
            imem_meta:write(egGameMsg, #egGameMsg{time=EventTime, gid=GameId, msgtype=MessageType, message=Message});
        false ->
            ok  % no notifications/logs needed for games bot against bot
    end.

-spec resume_bots(egGameTypeId(), [egBotId()]) -> ok | egGameError().
resume_bots(_GameTypeId, []) -> ok;
resume_bots(GameTypeId, [undefined|Bots]) ->
    resume_bots(GameTypeId, Bots);
resume_bots(GameTypeId, [Bot|Bots]) ->
    case global:whereis_name(?BOT_GID(Bot, GameTypeId)) of
        undefined -> 
            case Bot:resume(GameTypeId) of
                ok ->       resume_bots(GameTypeId, Bots);
                Error ->    Error
            end;
        Pid when is_pid(Pid) -> 
            resume_bots(GameTypeId, Bots)
    end.

-spec play_bot(egBotId(), egGameTypeId(), egGameId(), binary(), [egAlias()]) -> {ok, integer(), binary()} | {error, atom()}.
play_bot(BotId, GameTypeId, GameId, Board, Aliases) ->
        gen_server:cast(?BOT_GID(BotId, GameTypeId), {play_bot_req, GameId, Board, Aliases}).

%% stateless (db direct access) functions

-spec eg_time() -> egTime().
eg_time() -> imem_meta:time_uid().

-spec eg_time_to_sec(egTime()) -> undefined |integer().
eg_time_to_sec(undefined) -> undefined;
eg_time_to_sec({Sec,_,_,_}) -> Sec;
eg_time_to_sec({Sec,_}) -> Sec.

-spec eg_time_to_msec(egTime()) -> undefined |integer().
eg_time_to_msec(undefined) -> undefined;
eg_time_to_msec({Sec,Micro,_,_}) -> 1000*Sec+Micro div 1000;
eg_time_to_msec({Sec,Micro}) -> 1000*Sec+Micro div 1000.

-spec eg_time_to_usec(egTime()) -> undefined |integer().
eg_time_to_usec(undefined) -> undefined;
eg_time_to_usec({Sec,Micro,_,_}) -> 1000000*Sec+Micro;
eg_time_to_usec({Sec,Micro}) -> 1000000*Sec+Micro.

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

-spec resume(egGameId() | [egGameId()]) -> ok | egGameError().
resume(GameIds) when is_list(GameIds) ->
    case lists:usort([ resume(ID) || ID <- GameIds]) of
        [ok] -> ok;
        Errlist -> {error, Errlist}
    end;
resume(GameId)  ->
    case read_game(GameId) of
        #egGame{tid=GameType, bots=Bots} ->
            case resume_bots(GameType, Bots) of   
                ok ->
                    case read_type(GameType) of
                        #egGameType{engine=Engine} ->   Engine:resume(GameId);
                        Error ->                        Error
                    end;
                Error ->
                    Error
            end;   
        Error ->
            Error
    end.  

-spec stop(egGameId() | [egGameId()]) -> ok | egGameError().
stop(GameIds) when is_list(GameIds) ->
    case lists:usort([ stop(ID) || ID <- GameIds]) of
        [ok] -> ok;
        Errlist -> {error, Errlist}
    end;
stop(GameId)  ->
    case read_game(GameId) of
        #egGame{tid=GameType} ->   
            case read_type(GameType) of
                #egGameType{engine=Engine} ->   Engine:stop(GameId);
                Error ->                        Error
            end;   
        Error ->
            Error
    end.  

-spec result(egGameId()) -> egGameResult() | egGameError().
result(GameId) -> 
    try 
        gen_server:call(?ENGINE_GID(GameId), result)
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
        gen_server:call(?ENGINE_GID(GameId), moves)
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

-spec play(egGameId(), egGameMove(), egAlias(), egAccountId()) -> ok | game_finished | egGameError().
play(GameId, Move, MyAlias, MyAccountId) -> 
    engine_call(GameId, {play, Move, MyAlias, MyAccountId}).

-spec play(egGameId(), egGameMove()) -> ok | game_finished | egGameError().
play(GameId, Move) -> 
    engine_call(GameId, {play, Move}).

engine_call(GameId, Command) -> 
    try 
        gen_server:call(?ENGINE_GID(GameId), Command)
    catch
        exit:{normal,_} -> game_finished;       
        exit:{noproc,_} ->        
            case resume(GameId) of 
                ok ->   
                    try 
                        gen_server:call(?ENGINE_GID(GameId), Command)
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

insert_default([]) -> ok;
insert_default([Rec|Recs]) ->
    case imem_meta:read(element(1,Rec), element(2,Rec)) of
        [] ->   catch imem_meta:write(element(1,Rec), Rec);
        _ ->    ok
    end,
    insert_default(Recs).

init(_) ->
    Result = try
        imem_meta:init_create_table(egGameCategory, {record_info(fields, egGameCategory), ?egGameCategory, #egGameCategory{}}, [], system),  
        imem_meta:init_create_table(egGameType, {record_info(fields, egGameType), ?egGameType, #egGameType{}}, [], system),  
        imem_meta:init_create_table(egGame, {record_info(fields, egGame), ?egGame, #egGame{}}, [], system),  
        imem_meta:init_create_check_table(egGameMsg, {record_info(fields, egGameMsg), ?egGameMsg, #egGameMsg{}}, ?EG_MSG_TABLE_OPTS, system),
        insert_default(?DEFAULT_GAME_CATEGORIES),
        insert_default(?DEFAULT_GAME_TYPES),
        insert_default(?DEFAULT_ACCOUNTS),
        insert_default(?DEFAULT_ROLES),
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

handle_call({create, GameType, MyAccountId}, _From, State) when is_binary(GameType), is_integer(MyAccountId) ->
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
    % If proposed YourAccountId is a bot, it will automatically be accepted (Status=playing)
    case imem_meta:read(egGameType, GameType) of
        [#egGameType{cid=Cid}] ->
            % ToDo: check for existing account               
            GameId = rand:uniform(1844674407370955200),
            Game = #egGame{ gid=GameId
                          , tid=GameType
                          , cid=Cid
                          , players=[MyAccountId, YourAccountId]
                          , ctime=eg_time()
                          },
            MyBot = read_bot(MyAccountId),
            case read_bot(YourAccountId) of 
                undefined ->
                    write_game(Game#egGame{bots=[MyBot, undefined]}),
                    {reply, GameId, State};
                YourBot ->
                    case resume_bots(GameType, [MyBot, YourBot]) of
                        ok ->
                            save_resume(prepare(Game#egGame{bots=[MyBot, YourBot], status=playing, stime=eg_time()})),
                            {reply, GameId, State};
                        Error -> 
                            {reply, Error, State}
                    end
            end;
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
            #egGame{gid=GameId, bots=Bots} = Game = lists:nth(rand:uniform(length(Games)), Games),   % pick one game at random
            case resume_bots(GameType, Bots) of
                ok ->
                    save_resume(prepare(Game#egGame{status=playing, stime=eg_time()})),
                    {reply, GameId, State};
                Error ->
                    {reply, Error, State}
            end
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
            Bots = [B, read_bot(MyAccountId)],
            case resume_bots(GameType, Bots) of
                ok ->   
                    save_resume(prepare(Game#egGame{players=[P, MyAccountId], bots=Bots, status=playing, stime=eg_time()})),
                    {reply, Game#egGame.gid, State};
                Error ->
                    {reply, Error, State}
            end
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
            case resume_bots(GameType, Game#egGame.bots) of
                ok ->   
                    save_resume(prepare(Game#egGame{status=playing, stime=eg_time()})),
                    {reply, Game#egGame.gid, State};
                Error ->
                    {reply, Error, State}
            end
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
        [#egGame{status=forming, tid=GameType, players=[_, MyAccountId], bots=Bots} = Game] ->
            case resume_bots(GameType, Bots) of
                ok ->   
                    save_resume(prepare(Game#egGame{status=playing, stime=eg_time()})), 
                    {reply, ok, State};
                Error ->
                    {reply, Error, State}
            end;
        [#egGame{status=forming, players=[MyAccountId, _]}] ->               
            {reply, ?UNIQUE_PLAYERS, State};
        [#egGame{status=forming, players=[MyAccountId]}] ->               
            {reply, ?UNIQUE_PLAYERS, State};
        [#egGame{status=forming, tid=GameType, players=[Player], bots=[Bot]} = Game] ->
            Bots = [Bot, read_bot(MyAccountId)],
            case resume_bots(GameType, Bots) of
                ok ->
                    save_resume(prepare(Game#egGame{ status=playing
                                                 , players=[Player, MyAccountId]
                                                 , bots=Bots
                                                 , stime=eg_time()
                                                 }
                                     )),               
                    {reply, ok, State};
                Error ->
                    {reply, Error, State}
            end;
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
% egambo_game:create(<<"tic_tac_toe">>, 10, 1,4).

