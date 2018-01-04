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

-export([ play_bot_immediate_win/9
        , play_bot_defend_immediate/9
        , play_bot_random/9
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
    Options = egambo_tictac:put_options(Board, Width, Height, Gravity),
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

handle_info(Reqest, State) when element(1, Reqest) == notify_bot_req ->
    % ignore game notifications 
    {noreply, State};
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
    case supervisor:start_child(egambo_bot_sup, ChildSpec) of
        {ok,_} ->                       ok;
        {ok,_,_} ->                     ok;
        {error, already_present} ->     ok;
        {error,{already_started,_}} ->  ok;
        Error ->                        Error
    end.

-spec stop(egBotId()) -> ok | egGameError().
stop(GameTypeId) ->
    supervisor:terminate_child(egambo_bot_sup, ?BOT_ID(?MODULE, GameTypeId)),
    supervisor:delete_child(egambo_bot_sup, ?BOT_ID(?MODULE, GameTypeId)).


state(GameTypeId) ->
    gen_server:call(?BOT_GID(?MODULE, GameTypeId), state). 

-spec play_bot_random(binary(), integer(), integer(), integer(), boolean(), boolean(), egWinId(), [egAlias()], Options::[egGameMove()]) -> egBotMove() | {error, atom()}.
play_bot_random(Board, Width, _Height, _Run, Gravity, _Periodic, _WinMod, [Player|_], Options) when length(Options)==size(Board) ->
    egambo_tictac:put(Gravity, Board, Width, lists:nth(rand:uniform(length(Options)), Options), Player);
play_bot_random(Board, Width, Height, _Run, Gravity, Periodic, _WinMod, [Player|Others], Options) ->
    Idx = case rand:uniform(3) of
        1 -> % truly random
            lists:nth(rand:uniform(length(Options)), Options);
        2 -> % maximum connectivity = minimum availability connectivity
            ObNC = opts_by_neighbour_count(Board, Width, Height, Gravity, Periodic, Options, hd(Others)),
            % ?Info("ObNC others ~p",[ObNC]),
            element(2, lists:last(ObNC));
        3 -> % maximum own connectivity
            ObNC = opts_by_neighbour_count(Board, Width, Height, Gravity, Periodic, Options, Player),
            % ?Info("ObNC Player ~p",[ObNC]),
            element(2, lists:last(ObNC))
    end,
    egambo_tictac:put(Gravity, Board, Width, Idx, Player).

opts_by_neighbour_count(Board, Width, Height, Gravity, Periodic, Options, Player) ->
    opts_by_neighbour_count(Board, Width, Height, Gravity, Periodic, Options, Player,[]).

opts_by_neighbour_count(_, _, _, _, _, [], _, Acc) -> lists:sort(Acc);
opts_by_neighbour_count(Board, Width, Height, Gravity, Periodic, [Opt|Options], Player, Acc) ->
    Next = {{sig(neighbour_count(Board, Width, Height, Gravity, Periodic, Opt, Player)), rand:uniform()}, Opt}, 
    opts_by_neighbour_count(Board, Width, Height, Gravity, Periodic, Options, Player, [Next|Acc]).

neighbour_count(Board, Width, Height, false, Periodic, Opt, Player) ->
      neighbour_match(Board, Width, Height, Periodic, Opt, -1,  0, Player)
    + neighbour_match(Board, Width, Height, Periodic, Opt, +1,  0, Player)
    + neighbour_match(Board, Width, Height, Periodic, Opt,  0, -1, Player)
    + neighbour_match(Board, Width, Height, Periodic, Opt,  0, +1, Player)
    + neighbour_match(Board, Width, Height, Periodic, Opt, -1, -1, Player)
    + neighbour_match(Board, Width, Height, Periodic, Opt, -1, +1, Player)
    + neighbour_match(Board, Width, Height, Periodic, Opt, +1, -1, Player)
    + neighbour_match(Board, Width, Height, Periodic, Opt, +1, +1, Player);
neighbour_count(Board, Width, Height, true, Periodic, Opt, Player) ->
    {ok, Idx, _} = egambo_tictac:put(true, Board, Width, Opt, Player),
    neighbour_count(Board, Width, Height, false, Periodic, Idx, Player).

neighbour_match(_Board, Width, _Height, false, Pos, _, -1, ?AVAILABLE) when Pos<Width-> 1; 
neighbour_match(_Board, Width, _Height, false, Pos, _, -1, _Player) when Pos<Width -> 0;
neighbour_match(_Board, Width,  Height, false, Pos, _, +1, ?AVAILABLE) when Pos+Width >= Width*Height -> 1; 
neighbour_match(_Board, Width,  Height, false, Pos, _, +1, _Player) when Pos+Width >= Width*Height -> 0;
neighbour_match(_Board, Width, _Height, false, Pos, -1, _, ?AVAILABLE) when Pos rem Width == 0 -> 1; 
neighbour_match(_Board, Width, _Height, false, Pos, -1, _, _Player) when Pos rem Width == 0 -> 0;
neighbour_match(_Board, Width, _Height, false, Pos, +1, _, ?AVAILABLE) when Pos rem Width == Width-1 -> 1; 
neighbour_match(_Board, Width, _Height, false, Pos, +1, _, _Player) when Pos rem Width == Width-1 -> 0;
neighbour_match(Board, Width, _Height, false, Pos, DX, DY, Player) ->
    case lists:nth(1+Pos+DX+DY*Width, binary_to_list(Board)) of
        Player ->   1;
        _ ->        0
    end;
neighbour_match(Board, Width, Height, true, Pos, DX, DY, Player) ->
    NewX = (Pos+DX+Width) rem Width,
    NewY = (Pos div Height + DY + Height) div Height,
    case lists:nth(1+NewX+NewY*Width, binary_to_list(Board)) of
        Player ->   1;
        _ ->        0
    end.

sig(0) -> 0;
sig(X) when X>0 -> 1;
sig(X) when X<0 -> -1.

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
    case egambo_tictac:is_win(WinMod, TestBoard, Others) of
        true ->     egambo_tictac:put(Gravity, Board, Width, Idx, Player);
        false ->    play_bot_defend_immediate(Board, Width, Height, Run, Gravity, Periodic, WinMod, [Player|Others], Rest)
    end.
