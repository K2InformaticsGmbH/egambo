-module(egambo_tictac_bot).

-include("egambo_tictac.hrl").  % import tictac game definitions 

-behavior(gen_server).          % callbacks provided by (bot) players
%-behavior(egambo_gen_player).   % callbacks provided by (bot) players

-record(state,  { bid        :: egBotId()       % bot id (= module name)
                , tid        :: egGameTypeId()  % game type id
                , engine     :: egEngine()      % game engine (rule engine)
                , width      :: integer()       % board width >= 3
                , height     :: integer()       % board height >= 3
                , run        :: integer()       % sucess run length
                , gravity    :: boolean()       % do moves fall towards higher row numbers
                , periodic   :: boolean()       % unbounded repeating board
                , winmod     :: egWinId()       % win function module
                , players    :: integer()       % number of players 
                , status     :: egBotStatus()   % current bot status (e.g. learning / playing)
                }).

% gen_server behavior callback exports

-export([ start_link/1
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

% egambo_gen_engine behavior callbacks, used by eg_game (game manager)
-export([ resume/1      % instruct supervisor to start the bot process and resume the game
        , stop/1        % instruct supervisor to stop the bot process (no attempt to save the state)
        , game_types/1  % return list of supported game types for given game engine (or atom 'all') 
        ]).

% debugging API
-export([ state/1
        ]).

game_types(egambo_tictac) -> all;
game_types(_) -> [].

init([GameTypeId]) ->
    Result = try
        #egGameType{engine=Engine, players=Players, params=Params} = egambo_game:read_type(GameTypeId),
        Width=maps:get(width, Params),
        Height=maps:get(height, Params),
        Run=maps:get(run, Params),
        Gravity=maps:get(gravity, Params),
        Periodic=maps:get(periodic, Params),           
        State = #state{ bid=?MODULE 
                      , tid=GameTypeId
                      , engine=Engine
                      , width=Width
                      , height=Height
                      , run=Run
                      , gravity=Gravity
                      , periodic=Periodic
                      , winmod=egambo_tictac:win_module(Width, Height, Run, Periodic)
                      , players=Players
                      , status=playing
                      },
        process_flag(trap_exit, true),
        {ok, State}
    catch
        _Class:Reason -> {stop, {Reason,erlang:get_stacktrace()}} 
    end,
    Result.

start_link(GameTypeId)  ->
    gen_server:start_link(?BOT_GID(?MODULE, GameTypeId), ?MODULE, [GameTypeId], []).

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_cast({play_bot_req, GameId, _, [Player|_]}, #state{status=Status} = State) when Status /= playing ->
    play_bot_resp(GameId, Player, ?BOT_NOT_PLAYING),
    {noreply, State};
handle_cast({play_bot_req, GameId, Board, Aliases}, #state{ width=Width, height=Height, run=Run
                                                          , gravity=Gravity, periodic=Periodic, winmod=WinMod
                                                          } = State) -> 
    Options = put_options(Board, Width, Height, Gravity),
    case play_bot_immediate_win(Board, Width, Height, Run, Gravity, Periodic, WinMod, Aliases, Options) of
        {ok, Idx, NewBoard} ->              % win in this move detected
            play_bot_resp(GameId, hd(Aliases), {ok, Idx, NewBoard}); 
        {nok, no_immediate_win} ->
            case play_bot_defend_immediate(Board, Width, Height, Run, Gravity, Periodic, WinMod, Aliases, Options) of
                {ok, Idx, NewBoard} ->      % opponent's win in this move detected and taken
                    play_bot_resp(GameId, hd(Aliases), {ok, Idx, NewBoard});
                {nok, no_immediate_risk} ->
                    play_bot_resp(GameId, hd(Aliases), play_bot_random(Board, Width, Height, Run, Gravity, Periodic, WinMod, Aliases, Options));
                Error -> 
                    play_bot_resp(GameId, hd(Aliases), Error)
            end;
        Error -> 
            play_bot_resp(GameId, hd(Aliases), Error)
    end,
    {noreply, State};
handle_cast(Request, State) -> 
    ?Info("Unsolicited handle_cast in ~p : ~p",[?MODULE, Request]),
    {noreply, State}.

handle_info(Request, State) -> 
    ?Info("Unsolicited handle_info in ~p : ~p",[?MODULE, Request]),
    {noreply, State}.

handle_call(state, _From, State) ->
    {reply, State, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

-spec play_bot_resp(egGameId(), egAlias(), egBotMove() ) -> ok.
play_bot_resp(GameId, Player, BotMove) -> 
    send_to_engine(GameId, {play_bot_resp, Player, BotMove}).

-spec send_to_engine(egGameId(), egBotMoveCmd()) -> ok.
send_to_engine(GameId, Message) ->
    try 
        global:send(?ENGINE_ID(GameId), Message),
        ok
    catch 
        exit:{badarg, {_, _}} -> ?Error("GameEngine ~p unreachable. Dropped message: ~p",[GameId, Message])
    end.

-spec resume(egGameTypeId()) -> ok | egGameError().
resume(GameTypeId) ->
    ChildSpec = { ?BOT_ID(?MODULE, GameTypeId)                  % ChildId
                , {?MODULE, start_link, [GameTypeId]}           % {M,F,A}
                , permanent                                     % do restart automatically
                , 1000                                          % Shutdown timeout
                , worker                                        % Type
                , [?MODULE]                                     % Modules
                },
    case supervisor:start_child(egambo_sup, ChildSpec) of
        {ok,_} ->                       ok;
        {ok,_,_} ->                     ok;
        {error, already_present} ->     ok;
        {error,{already_started,_}} ->  ok;
        Error ->                        Error
    end.

-spec stop(egBotId()) -> ok | egGameError().
stop(GameTypeId) ->
    supervisor:terminate_child(egambo_sup, ?BOT_ID(?MODULE, GameTypeId)),
    supervisor:delete_child(egambo_sup, ?BOT_ID(?MODULE, GameTypeId)).


state(GameTypeId) ->
    gen_server:call(?BOT_GID(?MODULE, GameTypeId), state). 

random_idx1(Length) -> crypto:rand_uniform(1, Length+1). % 1..Length / 1 based random integer

-spec play_bot_random(binary(), integer(), integer(), integer(), boolean(), boolean(), egWinId(), [egAlias()], Options::[egGameMove()]) -> egBotMove() | {error, atom()}.
play_bot_random(Board, Width, _Height, _Run, Gravity, _Periodic, _WinMod, [Player|_], Options) ->
    egambo_tictac:put(Gravity, Board, Width, lists:nth(random_idx1(length(Options)), Options), Player).

-spec play_bot_immediate_win(binary(), integer(), integer(), integer(), boolean(), boolean(), egWinId(), [egAlias()], Options::[egGameMove()]) -> egBotMove() | {nok, no_immediate_win} | {error, atom()}.
play_bot_immediate_win(_Board, _Width, _Height, _Run, _Gravity, _Periodic, _WinMod, _Aliases, []) -> {nok, no_immediate_win};  
play_bot_immediate_win(Board, Width, Height, Run, Gravity, Periodic, WinMod, Aliases, [I|Rest]) -> 
    {ok, Idx, TestBoard} = egambo_tictac:put(Gravity, Board, Width, I, hd(Aliases)),
    case egambo_tictac:is_win(WinMod, TestBoard, Aliases) of
        true ->     {ok, Idx, TestBoard};
        false ->    play_bot_immediate_win(Board, Width, Height, Run, Gravity, Periodic, WinMod, Aliases, Rest)
    end.

-spec play_bot_defend_immediate(binary(), integer(), integer(), integer(), boolean(), boolean(), egWinId(), [egAlias()], Options::[egGameMove()]) -> egBotMove() | {nok, no_immediate_risk} | {error, atom()}.
play_bot_defend_immediate(_Board, _Width, _Height, _Run, _Gravity, _Periodic, _WinMod, _Aliases, []) -> {nok, no_immediate_risk};
play_bot_defend_immediate(Board, Width, Height, Run, Gravity, Periodic, WinMod, [Player|Others], [I|Rest]) -> 
    {ok, Idx, TestBoard} = egambo_tictac:put(Gravity, Board, Width, I, hd(Others)),
    case WinMod:win(TestBoard, Others) of
        true ->     egambo_tictac:put(Gravity, Board, Width, Idx, Player);
        false ->    play_bot_defend_immediate(Board, Width, Height, Run, Gravity, Periodic, WinMod, [Player|Others], Rest)
    end.

put_options(Board, Width, Height, false) ->
    lists:usort([ case B of ?AVAILABLE -> I; _ -> false end || {B,I} <- lists:zip(binary_to_list(Board), lists:seq(0,Width*Height-1))]) -- [false];
put_options(Board, Width, _Height, true) ->
    lists:usort([ case B of ?AVAILABLE -> I; _ -> false end || {B,I} <- lists:zip(binary_to_list(binary:part(Board,0,Width)), lists:seq(0,Width-1))]) -- [false].
