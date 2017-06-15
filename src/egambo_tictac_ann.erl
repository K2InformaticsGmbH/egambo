-module(egambo_tictac_ann).

-include("egambo_tictac.hrl").  % import tictac game definitions 

-behavior(gen_server).          % callbacks provided by (bot) players
%-behavior(egambo_gen_player).   % callbacks provided by (bot) players

-record(egTicTacAnn, { id         :: {egBotId(), egGameTypeId()}
                     , version = <<>>   :: binary()    % network version name
                     , info     = <<>>  :: binary()    % network description / res
                     , time = undefined :: ddTimeUID() % save time
                     , layers = []      :: list()      % number of layers and neurons per layer
                     , weights = []     :: list()      % neuron weights
                     }).

-define(egTicTacAnn, [ tuple
                     , binstr
                     , binstr
                     , list
                     , list
                     ]).
% rd(egTicTacAnn, {id=, version=, info=, time= layers=, weights=}).

-record(state,  { bid        :: egBotId()       % bot id (= module name)
                , tid        :: egGameTypeId()  % game type id
                , engine     :: egEngine()      % game engine (rule engine)
                , width=3    :: integer()       % board width >= 3
                , height=3   :: integer()       % board height >= 3
                , run=3      :: integer()       % sucess run length
                , gravity=false :: boolean()    % do moves fall towards higher row numbers
                , periodic=false :: boolean()   % unbounded repeating board
                , winmod     :: egWinId()       % win function module
                , players=2  :: integer()       % number of players 
                , status     :: egBotStatus()   % current bot status (e.g. learning / playing)
                , layers=[]  :: [integer()]     % neuron count per  layer
                , rate=0.05  :: float()         % default learning rate
                , epochs=10  :: integer()       % default learning epochs
                , error=0.001 :: float()        % default learning error
                , network  :: undefined | pid() % network root process
                , version= <<>> :: binary()       % network version name
                , info= <<>> :: binary()       % network info
                , gid=0      :: egGameId()
                , board= <<>> :: binary()  
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
-export([ resume/1      % instruct supervisor to start the bot process and resume learning/playing
        , stop/1        % instruct supervisor to stop the bot process (no attempt to save the state)
        , game_types/1  % return list of supported game types for given game engine (or atom 'all') 
        ]).

% learning and debugging API
-export([ state/1
        , start_learning/1
        , start_playing/1
        , learn_epochs/4
        , learn_until/4
        , save/1
        , save/2
        , save/3
        ]).

-define(BOT_IS_BUSY, {error, bot_is_busy}).

game_types(egambo_tictac) -> all;
game_types(_) -> [].

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

learn_epochs(GameTypeId, Epochs, Rate, TrainingView) ->
    gen_server:cast(?BOT_GID(?MODULE, GameTypeId), {learn_epochs, Epochs, Rate, TrainingView}).

learn_until(GameTypeId, Tolerance, Rate, TrainingView) ->
    gen_server:cast(?BOT_GID(?MODULE, GameTypeId), {learn_until, Tolerance, Rate, TrainingView}).

-spec stop(egBotId()) -> ok | egGameError().
stop(GameTypeId) ->
    gen_server:call(?BOT_GID(?MODULE, GameTypeId), finish), 
    supervisor:terminate_child(egambo_sup, ?BOT_ID(?MODULE, GameTypeId)),
    supervisor:delete_child(egambo_sup, ?BOT_ID(?MODULE, GameTypeId)).

state(GameTypeId) ->
    gen_server:call(?BOT_GID(?MODULE, GameTypeId), state). 

save(GameTypeId) ->
    gen_server:call(?BOT_GID(?MODULE, GameTypeId), save). 

save(GameTypeId, Version) ->
    gen_server:call(?BOT_GID(?MODULE, GameTypeId), {save, Version}). 

save(GameTypeId, Version, Info) ->
    gen_server:call(?BOT_GID(?MODULE, GameTypeId), {save, Version, Info}). 

start_learning(GameTypeId) ->
    gen_server:call(?BOT_GID(?MODULE, GameTypeId), start_learning). 

start_playing(GameTypeId) ->
    gen_server:call(?BOT_GID(?MODULE, GameTypeId), start_playing). 

init([GameTypeId]) ->
    Result = try
        imem_meta:init_create_table(egTicTacAnn, {record_info(fields, egTicTacAnn), ?egTicTacAnn, #egTicTacAnn{}}, [], system),  
        #egGameType{engine=Engine, players=Players, params=Params} = egambo_game:read_type(GameTypeId),
        Width=maps:get(width, Params),
        Height=maps:get(height, Params),
        Run=maps:get(run, Params),
        Gravity=maps:get(gravity, Params),
        Periodic=maps:get(periodic, Params),
        {Layers, Network, Status, Version, Info} = case imem_meta:read(egTicTacAnn, ?BOT_ID(?MODULE, GameTypeId)) of
            [] ->
                L = [Width*Height|[Width*Height || _ <- lists:seq(2, Width)]],  
                {L, ann:create_neural_network(L),learning, <<>>, <<>>};
            [#egTicTacAnn{layers=L, weights=W, version=V, info=I}] ->
                {L, ann:create_neural_network(L, W), playing, V, I}
        end,
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
                      , status=Status
                      , layers=Layers
                      , version=Version
                      , info=Info
                      , network=Network
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
handle_cast({play_bot_req, GameId, _, [Player|_]}, #state{gid=GameId} = State) when GameId /= undefined ->
    play_bot_resp(GameId, Player, ?BOT_IS_BUSY),
    {noreply, State};
handle_cast({play_bot_req, GameId, Board, Aliases}, #state{ width=Width, height=Height, run=Run, network=Network
                                                          , gravity=Gravity, periodic=Periodic, winmod=WinMod
                                                          } = State) -> 
    Options = put_options(Board, Width, Gravity),
    case play_bot_immediate_win(Board, Width, Height, Run, Gravity, Periodic, WinMod, Aliases, Options) of
        {ok, Idx, NewBoard} ->              % win in this move detected
            play_bot_resp(GameId, hd(Aliases), {ok, Idx, NewBoard}),
            {noreply, State}; 
        {nok, no_immediate_win} ->
            case play_bot_defend_immediate(Board, Width, Height, Run, Gravity, Periodic, WinMod, Aliases, Options) of
                {ok, Idx, NewBoard} ->      % opponent's win in this move detected and taken
                    play_bot_resp(GameId, hd(Aliases), {ok, Idx, NewBoard}),
                    {noreply, State};
                {nok, no_immediate_risk} ->
                    play_bot_ann(Board, Aliases, Network),
                    {noreply, State#state{gid=GameId, board=Board, players=Aliases}};
                Error -> 
                    play_bot_resp(GameId, hd(Aliases), Error),
                    {noreply, State}
            end;
        Error -> 
            play_bot_resp(GameId, hd(Aliases), Error),
            {noreply, State}
    end;
handle_cast({learn_until, Tolerance, Rate, TrainingSet}, #state{network=Network, status=learning} = State) ->
    Network ! {learn_until, Tolerance, Rate, TrainingSet},
    {noreply, State};
handle_cast({learn_epochs, Epochs, Rate, TrainingSet}, #state{network=Network, status=learning} = State) ->
    Network ! {learn_epochs, Epochs, Rate, TrainingSet},
    {noreply, State};
handle_cast(Request, State) -> 
    ?Info("Unsolicited handle_cast in ~p : ~p",[?MODULE, Request]),
    {noreply, State}.

handle_info({predicted, Output}, #state{gid=GameId, board=Board, width=Width, gravity=Gravity, players=[Player|_] } = State) ->
    BotMove = pick_output(Output, Board, Width, Gravity, Player),
    play_bot_resp(GameId, Player, BotMove),
    {noreply, State#state{gid=undefined}};    
handle_info(Request, State) -> 
    ?Info("Unsolicited handle_info in ~p : ~p",[?MODULE, Request]),
    {noreply, State}.

handle_call(finish, _From, #state{network=Network} = State) ->
    Network ! finish,
    {reply, ok, State#state{network=undefined}};
handle_call(start_learning, _From, State) ->
    {reply, ok, State#state{status=learning}};
handle_call(start_playing, _From, State) ->
    {reply, ok, State#state{status=playing}};
handle_call(save, _From, #state{version=Version, info=Info} = State) ->
    handle_call({save, Version, Info}, _From, State);
handle_call({save, Version}, _From, #state{info=Info} = State) ->
    handle_call({save, Version, Info}, _From, State);
handle_call({save, Version, Info}, _From, #state{tid=GameTypeId, layers=Layers, network=Network} = State) ->
    Network = Network, 
    Weights = [], % Todo: ask Network
    imem_meta:write(egTicTacAnn, #egTicTacAnn{id=?BOT_ID(?MODULE, GameTypeId) , version=Version, info=Info, layers=Layers, weights=Weights}),
    {reply, ok, State};
handle_call(state, _From, State) ->
    {reply, State, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

pick_output(Output, Board, Width, Gravity, Player) ->
    SortedOutput = lists:sort([{Gain,I} || {Gain,I} <- lists:zip(Output, lists:seq(0, length(Output)-1))]),   
    pick_output_sorted(SortedOutput, Board, Width, Gravity, Player).

pick_output_sorted([], Board, Width, Gravity, Player) ->
    ?Info("Random move played by ~p in ~p for board ~p",[Player, ?MODULE, Board]),
    play_bot_random(Board, Width, Gravity, Player);
pick_output_sorted([{_,I}|Outputs], Board, Width, Gravity, Player) ->
    case egambo_tictac:put(Gravity, Board, Width, I, Player) of
        {ok, Idx, NewBoard} ->  {ok, Idx, NewBoard};
        _ ->                    pick_output_sorted(Outputs, Board, Width, Gravity, Player)
    end.

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

random_idx1(Length) -> crypto:rand_uniform(1, Length+1). % 1..Length / 1 based random integer

-spec play_bot_random(binary(), integer(), boolean(), egAlias()) -> {ok, Move::integer(), NewBoard::binary()} | {error, atom()}.
play_bot_random(Board, Width, Gravity, Player) ->
    Options = put_options(Board, Width, Gravity),
    egambo_tictac:put(Gravity, Board, Width, lists:nth(random_idx1(length(Options)), Options), Player).

-spec play_bot_ann(binary(), [egAlias()], pid()) -> ok | egGameError().
play_bot_ann(Board, [Player|_], Network) ->
    Network ! {predict, [Player|binary_to_list(Board)], self()}.

-spec play_bot_immediate_win(binary(), integer(), integer(), integer(), boolean(), boolean(), egWinId(), [egAlias()], Options::[egGameMove()]) -> {ok, integer(), binary()} | {nok, no_immediate_win} | {error, atom()}.
play_bot_immediate_win(_Board, _Width, _Height, _Run, _Gravity, _Periodic, _WinMod, _Aliases, []) -> {nok, no_immediate_win};  
play_bot_immediate_win(Board, Width, Height, Run, Gravity, Periodic, WinMod, Aliases, [I|Rest]) -> 
    {ok, Idx, TestBoard} = egambo_tictac:put(Gravity, Board, Width, I, hd(Aliases)),
    case WinMod:win(TestBoard, Aliases) of
        true ->     {ok, Idx, TestBoard};
        false ->    play_bot_immediate_win(Board, Width, Height, Run, Gravity, Periodic, WinMod, Aliases, Rest)
    end.

-spec play_bot_defend_immediate(binary(), integer(), integer(), integer(), boolean(), boolean(), egWinId(), [egAlias()], Options::[egGameMove()]) -> {ok, Move::integer(), NewBoard::binary()} | {nok, no_immediate_risk} | {error, atom()}.
play_bot_defend_immediate(_Board, _Width, _Height, _Run, _Gravity, _Periodic, _WinMod, _Aliases, []) -> {nok, no_immediate_risk};
play_bot_defend_immediate(Board, Width, Height, Run, Gravity, Periodic, WinMod, [Player|Others], [I|Rest]) -> 
    {ok, Idx, TestBoard} = egambo_tictac:put(Gravity, Board, Width, I, hd(Others)),
    case WinMod:win(TestBoard, Others) of
        true ->     egambo_tictac:put(Gravity, Board, Width, Idx, Player);
        false ->    play_bot_defend_immediate(Board, Width, Height, Run, Gravity, Periodic, WinMod, [Player|Others], Rest)
    end.

put_options(Board, _Width, false) ->
    lists:usort([ case B of ?AVAILABLE -> I; _ -> false end || {B,I} <- lists:zip(binary_to_list(Board), lists:seq(0,size(Board)-1))]) -- [false];
put_options(Board, Width, true) ->
    lists:usort([ case B of ?AVAILABLE -> I; _ -> false end || {B,I} <- lists:zip(binary_to_list(binary:part(Board,0,Width)), lists:seq(0,Width-1))]) -- [false].
