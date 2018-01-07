-module(egambo_tictac_bot_ql).

-include("egambo_tictac.hrl").  % import tictac game definitions 

-behavior(gen_server).          % callbacks provided by (bot) players
%-behavior(egambo_gen_player).   % callbacks provided by (bot) players

-define(QL_TRAIN, "egTicTacQl_").           % Q-table name prefix (egGameTypeId to be appended)
-define(QL_TRAIN_OPTS, [{record_name, egTicTacQlSample}, {type, set}]).        
-define(QL_OUTPUT_UNSAMPLED, 0.001).        % Value emitted as output for unsampled move in Export

-record(egTicTacQlSample,   { input= <<>> :: binary()  % binstr key of normalized position (Board)
                            , nos=1       :: integer() % number of board samples (1...)
                            , nmax=1      :: integer() % number of action samples (1...) for Qmax
                            , qmax=1      :: number()  % Qmax of best action (-1..+1) as a quotient
                            , aaq=[]    :: list()      % aggregated action qualities
                            }).

-define(egTicTacQlSample,   [ binstr
                            , integer
                            , integer
                            , number
                            , list
                            ]).
% rd(egTicTacQlSample, {input, nos, nmax, qmax, aaq}).

-record(state,  { bid        :: egBotId()       % bot id (= module name)
                , tid        :: egGameTypeId()  % game type id
                , engine     :: egEngine()      % game engine (rule engine)
                , width      :: integer()       % board width >= 3
                , height     :: integer()       % board height >= 3
                , run        :: integer()       % sucess run length
                , gravity    :: boolean()       % do moves fall towards higher row numbers
                , periodic   :: boolean()       % unbounded repeating board
                , ialiases   :: [egAlias()]     % normalized player order
                , winmod     :: egWinId()       % win function module
                , players    :: integer()       % number of players 
                , status     :: egBotStatus()   % current bot status (e.g. learning / playing)
                , explore=0.999 :: float()      % rate for exploration (0..1.0)
                , flatten=0.0 :: float()        % 
                , table      :: atom()          % table name for Q-A-aggregation
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
        , explore/1
        , explore/2
        , flatten/1
        , flatten/2
        , pick/4
        , play_bot_impl/11
        , train_game/8
        , qlearn_max/1
        ]).

-safe([state, stop, resume, explore, flatten, qlearn_max, pick, play_bot_impl]).

game_types(egambo_tictac) -> all;
game_types(_) -> [].

% Samples=egambo_tictac:samples(<<"         ">>, <<"XO">>, Moves, Naliases, Nscores).
% egambo_tictac_bot_ql:train_game(Samples, Table, 3, 3, false, false, 0.9, undefined).

init([GameTypeId]) ->
    Table = list_to_atom(?QL_TRAIN ++ binary_to_list(GameTypeId)),
    imem_meta:init_create_table(Table, {record_info(fields, egTicTacQlSample), ?egTicTacQlSample, #egTicTacQlSample{}}, ?QL_TRAIN_OPTS, system),  
    Result = try
        #egGameType{engine=Engine, players=Players, params=Params} = egambo_game:read_type(GameTypeId),
        Width=maps:get(width, Params),
        Height=maps:get(height, Params),
        Run=maps:get(run, Params),
        Periodic=maps:get(periodic, Params),
        State = #state{ bid=?MODULE 
                      , tid=GameTypeId
                      , engine=Engine
                      , width=Width
                      , height=Height
                      , run=Run
                      , gravity=maps:get(gravity, Params)
                      , periodic=Periodic
                      , ialiases=maps:get(aliases, Params)
                      , winmod=egambo_tictac:win_module(Width, Height, Run, Periodic)
                      , players=Players
                      , status=playing
                      , table = Table
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

explore(GameTypeId) ->
    gen_server:call(?BOT_GID(?MODULE, GameTypeId), {explore}).

explore(GameTypeId, Fraction) when is_number(Fraction)->
    gen_server:call(?BOT_GID(?MODULE, GameTypeId), {explore, Fraction}).

flatten(GameTypeId) ->
    gen_server:call(?BOT_GID(?MODULE, GameTypeId), {flatten}).

flatten(GameTypeId, Fraction)  when is_number(Fraction) ->
    gen_server:call(?BOT_GID(?MODULE, GameTypeId), {flatten, Fraction}).


handle_cast({play_bot_req, GameId, _, [Player|_]}, #state{status=Status} = State) when Status /= playing ->
    play_bot_resp(GameId, Player, ?BOT_NOT_PLAYING),
    {noreply, State};
handle_cast({play_bot_req, GameId, Board, NAliases}, #state{ width=Width, height=Height, run=Run
                                                          , gravity=Gravity, periodic=Periodic, winmod=WinMod
                                                          , ialiases=IAliases, explore=Explore, flatten=Flatten
                                                          , table=Table
                                                          } = State) -> 
    Options = egambo_tictac:put_options(Board, Width, Height, Gravity),
    case egambo_tictac_bot:play_bot_immediate_win(Board, Width, Height, Run, Gravity, Periodic, WinMod, NAliases, Options) of
        {ok, Idx, NewBoard} ->              % win in this move detected
            play_bot_resp(GameId, hd(NAliases), {ok, Idx, NewBoard}),
            {noreply, State}; 
        {nok, no_immediate_win} ->
            case egambo_tictac_bot:play_bot_defend_immediate(Board, Width, Height, Run, Gravity, Periodic, WinMod, NAliases, Options) of
                {ok, Idx, NewBoard} ->      % opponent's win in this move detected and taken
                    play_bot_resp(GameId, hd(NAliases), {ok, Idx, NewBoard});
                {nok, no_immediate_risk} ->
                    {ok, Idx, NewBoard} = play_bot_impl(Board, Width, Height, Run, Gravity, Periodic, IAliases, NAliases, Explore, Flatten, Table),
                    play_bot_resp(GameId, hd(NAliases), {ok, Idx, NewBoard});
                Error -> 
                    play_bot_resp(GameId, hd(NAliases), Error)
            end,
            {noreply, State};
        Error -> 
            play_bot_resp(GameId, hd(NAliases), Error),
            {noreply, State}
    end;
handle_cast(Request, State) -> 
    ?Info("Unsolicited handle_cast in ~p : ~p",[?MODULE, Request]),
    {noreply, State}.

handle_info({notify_bot_req, _GameId, Ialiases, Space, finished, Naliases, Nscores, Moves}=_Request
            , #state{ width=Width, height=Height, gravity=Gravity, periodic=Periodic
            , explore=Explore, table=Table} = State) ->
    % ?Info("received ~p",[_Request]),
    train_game(egambo_tictac:samples(Space, Ialiases, Moves, Naliases, Nscores), Table, Width, Height, Gravity, Periodic, Explore, undefined),
    {noreply, State};
handle_info(Reqest, State) when element(1, Reqest) == notify_bot_req ->
    % ignore game notifications 
    {noreply, State};
handle_info(Request, State) -> 
    ?Info("Unsolicited handle_info in ~p : ~p",[?MODULE, Request]),
    {noreply, State}.

handle_call({explore}, _From, State) ->
    {reply, State#state.explore, State};
handle_call({explore, Fraction}, _From, State) ->
    {reply, ok, State#state{explore=Fraction}};
handle_call({flatten}, _From, State) ->
    {reply, State#state.flatten, State};
handle_call({flatten, Fraction}, _From, State) ->
    {reply, ok, State#state{flatten=Fraction}};
handle_call(state, _From, State) ->
    {reply, State, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.


%% Scan the moves of a game from last to first and update an aggregated Q-learning table which can also be used to train an ANN (for interpolation)
train_game([], _Table, _Width, _Height, _Gravity, _Periodic, _Explore, _Qmax) -> ok;
train_game([Sample|Samples], Table, Width, Height, Gravity, Periodic, Explore, Qmax) ->
    NextQmax = train_move(Sample, Table, Width, Height, Gravity, Periodic, Explore, Qmax),
    train_game(Samples, Table, Width, Height, Gravity, Periodic, Explore, NextQmax).

%% Format a single (already Alias-normalized) move for ann use (Board size input vector, Board size / Board Width output vector)
%% and aggregate into the training Q table
%% Score is the final score of the game as seen by the current player
%% Qmax is the updated position quality (with respect to a win of X) for the next move (played after the current one)
%% Qmax is calculated from all Q(Board, Moves) as seen in the perspective of the opponent
%% The Q-learning back-propagation needs to negate this Q for the current player (neglect Joker subtleties for now)
train_move([_Board, _Players, _Move, [Score|_], 0], _Table, _Width, _Height, _, _Periodic, _Explore, undefined) ->
    % ?Info("ql train_move Board ~p and Move ~p for temporary X", [_Board, _Move]),
    % ?Info("ql train_move immediate reward ~p ", [Score]),
    % Do not record this pre-final board state. Decision will be taken by a one step look ahead, not from the Q-Table
    {1,Score}; % (Qmax of final move, +1 for a win or 0 for a tie)
train_move([Board, _Players, Move, _, MTE], Table, Width, Height, false, Periodic, Explore, Qmax) ->
    % ?Info("ql train_move Board ~p and Move ~p for temporary X", [Board, Move]),
    % ?Info("ql train_move target position qmax ~p ", [Qmax]),
    {Input, Sym} = egambo_tictac_sym:norm(Width, Height, false, Periodic, Board),
    % ?Info("ql train_move Input ~p and Sym ~p", [Input, Sym]),
    Olen = Width*Height,            % length of output vector (possible actions)
    OMove = egambo_tictac_sym:map_move(Width, Height, Move, Sym) + 1,   % one based index of move
    train(Input, sample_output(Input, Olen, OMove), MTE, Table, OMove, Olen, Explore, Qmax);
train_move([Board, _Players, Move, _, MTE], Table, Width, Height, true, Periodic, Explore, Qmax) ->
    % ?Info("ql train_move Board ~p and Move ~p for temporary X", [Board, Move]),
    % ?Info("ql train_move target position qmax ~p ", [Qmax]),
    {Input, Sym} = egambo_tictac_sym:norm(Width, Height, true, Periodic, Board),
    % ?Info("ql train_move Input and Sym ~p ~p", [Input, Sym]),
    Olen = Width,                   % length of output vector (possible actions)
    OMove = egambo_tictac_sym:map_move(Width, Height, Move rem Width, Sym) + 1,    % one based index to Output
    train(Input, sample_output(Input, Olen, OMove), MTE, Table, OMove, Olen, Explore, Qmax).

sample_output(Input, Olen, OMove) ->
    F = fun(I) -> 
        Inp=lists:nth(I, Input), 
        if 
            I==OMove ->  {1, 0};    % intermediate move taken, no immediate reward - immediate_q(MTE>0,Score)=0
            Inp==32 ->   {0, 0};    % alternative legal move, not taken here
            true ->      0          % illegal move (occupied)
        end 
    end, 
    Output = lists:map(F, lists:seq(1, Olen)),
    % ?Info("ql train_move Output ~p OutputIdx ~p", [Output, OMove]),
    Output.

train(Input, Output, MTE, Table, OMove, Olen, Explore, Qmax) when MTE>0 ->
    {PreviousSampleCount, AggregatedActionQualities} = case imem_meta:read(Table, list_to_binary(Input)) of
        [] ->   % TrainingRec does not exist yet. Cheat a bit, double count the first result.
            {0, qlearn_add(Output, Output, Qmax, OMove, Olen, Explore)}; 
        [#egTicTacQlSample{nos=N, aaq=OldOut}] ->
            {N, qlearn_add(OldOut, Output, Qmax, OMove, Olen, Explore)}
    end,
    {CN,CQ} = NewQmax = qlearn_max(AggregatedActionQualities),   
    TrainingRec=#egTicTacQlSample{input=list_to_binary(Input), nos=PreviousSampleCount+1, nmax=CN, qmax=CQ/CN, aaq=AggregatedActionQualities},
    imem_meta:write(Table, TrainingRec),
    NewQmax.

qlearn_add(OldOut, Output, NextOut, OMove, Olen, Explore) ->
    Alfa = Explore,
    Gamma = -Explore,
    [qlearn_add_one(Old, Out, Pos, NextOut, OMove, Alfa, Gamma) || {Old, Out, Pos} <- lists:zip3(OldOut, Output, lists:seq(1, Olen))].

%% Add one sample to the Q-learning accumulator (single output vector position)
%% accumulated Q:                   {AccN,AccQ}={Number of accumulated samples so far, sum of quality for this action}
%% immediate reward:                {N,Q}       (normally {1,0} for non-final moves, {0,0} for unsampled actions)
%% invalid moves (actions):         0 + 0 -> 0
qlearn_add_one(0, 0, _, _, _, _, _) -> 0;                           % invalid moves 
qlearn_add_one({AccN,AccQ}, {0,_}, _, _, _, _, _) -> {AccN,AccQ};   % unsampled actions
qlearn_add_one({0,0}, {N,Q}, OMove, Qmax, OMove, Alfa, Gamma) ->
    % Q-learn with immediate reward (Q=0 for TicTacChallenge)
    {CN,CQ} = Qmax,
    {N, Alfa*(Q + Gamma*N*CQ/CN)};
qlearn_add_one({AccN,AccQ}, {N,Q}, OMove, Qmax, OMove, Alfa, Gamma) ->
    % Q-learn with immediate reward (Q=0 for TicTacChallenge)
    {CN,CQ} = Qmax,
    AccNN = AccN+N,
    AccQN = AccNN*AccQ/AccN,
    {AccNN, AccQN + Alfa*(Q*AccNN/N + Gamma*CQ*AccNN/CN - AccQN)}.

qlearn_max(Output) -> qlearn_max(Output,0,-1.0e100).

qlearn_max([], Max, _) -> Max;  
qlearn_max([0|Output], Max, MaxQ) -> qlearn_max(Output, Max, MaxQ);     % invalid action
qlearn_max([{0,_}|Output], Max, MaxQ) -> qlearn_max(Output, Max, MaxQ); % unsampled action
qlearn_max([{N,Q}|Output], _Max, MaxQ) when Q/N>MaxQ -> qlearn_max(Output, {N,Q}, Q/N); % bigger
qlearn_max([{N,Q}|Output], {NMax,_}, MaxQ) when Q/N==MaxQ, N>NMax -> qlearn_max(Output, {N,Q}, Q/N); % more statistics
qlearn_max([{_,_}|Output], Max, MaxQ) -> qlearn_max(Output, Max, MaxQ). % smaller or equal

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


-spec play_bot_impl(binary(), integer(), integer(), integer(), boolean(), boolean(), [egAlias()], [egAlias()], float(), float(), atom()) -> egBotMove() | {error, atom()}.
play_bot_impl(Board, Width, Height, _Run, Gravity, Periodic, IAliases, NAliases, Explore, Flatten, Table) ->
    % ?Info("play_bot_impl Board, NAliases ~p ~p", [Board, NAliases]),
    ABoard = egambo_tictac:norm_aliases(Board, NAliases, IAliases),
    % ?Info("play_bot_impl ABoard after alias transform ~p ", [ABoard]),
    {Input, Sym} = egambo_tictac_sym:norm(Width, Height, Gravity, Periodic, ABoard), 
    % ?Info("play_bot_impl Input (normalized board) and symmetry used ~p ~p", [Input, Sym]),
    {NOS, AccActionQuality} = case imem_meta:read(Table, list_to_binary(Input)) of
        [] ->   {0, new_output(Input, Width, Height, Gravity)};
        [#egTicTacQlSample{nos=Nos, aaq=Aaq}] -> {Nos,Aaq}
    end,
    % ?Info("play_bot_impl ActionQuality ~p",[ActionQuality]),    
    NormIdx = pick(NOS, AccActionQuality, Explore, Flatten),      % 0-based index for predicted move
    NewIdx = egambo_tictac_sym:unmap_move(Width, Height, NormIdx, Sym),    
    % ?Info("play_bot_impl NormIdx, NewIdx (output picked) ~p ~p", [NormIdx, NewIdx]),
    {ok, Idx, NewBoard} = egambo_tictac:put(Gravity, Board, Width, NewIdx, hd(NAliases)),
    % ?Info("play_bot_impl Idx, NewBoard ~p ~p", [Idx, binary_to_list(NewBoard)]),
    {ok, Idx, NewBoard}.

new_output(Input, Width, Height, false) ->
    F = fun(I) -> 
        Inp=lists:nth(I, Input), 
        if 
            Inp==32 ->   {0, 0};      % alternative legal move
            true ->      0            % illegal move (occupied)
        end 
    end, 
    lists:map(F, lists:seq(1, Width*Height));
new_output(Input, Width, _Height, true) ->
    F = fun(I) -> 
        Inp=lists:nth(I, Input), 
        if 
            Inp==32 ->   {0, 0};      % alternative legal move
            true ->      0            % illegal move (occupied)
        end 
    end, 
    lists:map(F, lists:seq(1, Width)).

pick(NOS, AccActionQuality, Explore, 0) ->
    AL = [{target(A, NOS, Explore), rand:uniform(), I} || {A,I} <- lists:zip(AccActionQuality, lists:seq(0, length(AccActionQuality)-1))],
    element(3, lists:last(lists:sort(AL)));
pick(NOS, AccActionQuality, Explore, Flatten) ->
    AL = [{target(A, NOS, Explore) + Flatten* rand:uniform(), I} || {A,I} <- lists:zip(AccActionQuality, lists:seq(0, length(AccActionQuality)-1))],
    element(2, lists:last(lists:sort(AL))).

target(0, _, _) -> -100; 
target({0,0}, _, _) -> 100; 
target({N,Q}, NOS, Explore) -> 
    DE = 1.0 + Explore*math:sqrt(NOS)/(1+N),
    (Q/N + DE) / DE.
% target({N,Q}, NOS, Explore) -> Q/N + Explore*math:sqrt(NOS)/(1+N).
