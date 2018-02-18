-module(egambo_tictac_bot_ql).

-include("egambo_tictac.hrl").  % import tictac game definitions 

-behavior(gen_server).          % callbacks provided by (bot) players
%-behavior(egambo_gen_player).   % callbacks provided by (bot) players

-define(QL_TRAIN, "egTicTacQl_").           % Q-table name prefix (egGameTypeId to be appended)
-define(QL_TRAIN_OPTS, [{record_name, egTicTacQlSample}, {type, set}]).        

-define(UCB1_BONUS(__N, __Na, _, __Exp), 2*__Exp*math:sqrt(math:log(__N)/__Na)).    % from "A Survey of Monte Carlo Tree Search Methods"
-define(ALFAGO_BONUS(_, __Na, __P, __Exp), __Exp*__P/(1+__Na)).                     % from "Alfa Go Nature Paper"
-define(ALFAGOZERO_BONUS(__N, __P, __Na, __Exp), __Exp*__P*sqrt(__N)/(1+__Na)).     % from "Alfa Go Zero"

-type egTicTacQlActionQuality() :: {integer(),integer(),integer()} | 0.
-define(QL_AQ_INVALID, 0).      % Quality of invalid action
-define(QL_AQ_INIT, {0,0,0}).   % {Samples for this action, SN, SQ} where Quality = SQ/SN 

-record(egTicTacQlSample,   { input= <<>> :: binary()  % binstr key of normalized position (Board)
                            , nos=1       :: integer() % number of board samples (1...)
                            , nmax=1      :: integer() % number of action samples (1...) for Qmax
                            , qmax=1      :: number()  % Qmax of best action (-1..+1) as a quotient
                            , aaq=[]      :: list()    % aggregated action qualities
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
                , explore=1  :: number()        % rate for exploration (0..1)
                , flatten=0  :: number()        % flat peak randomisation
                , lrate=2    :: number()        % learning rate
                , discount=1 :: number()        % Q-discount per move
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
        , lrate/1
        , lrate/2
        , discount/1
        , discount/2
        , pick/5
        , play_bot_impl/11
        , train_game/9
        , qlearn_max/1
        ]).

-safe([state, stop, resume, explore, flatten, lrate, discount, qlearn_max, pick, play_bot_impl]).

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

call(GameTypeId, Request) ->
    gen_server:call(?BOT_GID(?MODULE, GameTypeId), Request).

state(GameTypeId) -> call(GameTypeId, state).

explore(GameTypeId) -> call(GameTypeId, {explore}).

explore(GameTypeId, Fraction) when is_number(Fraction) -> call(GameTypeId, {explore, Fraction}).

flatten(GameTypeId) -> call(GameTypeId, {flatten}).

flatten(GameTypeId, Fraction)  when is_number(Fraction) -> call(GameTypeId, {flatten, Fraction}).

lrate(GameTypeId) -> call(GameTypeId, {lrate}).

lrate(GameTypeId, Fraction)  when is_number(Fraction) -> call(GameTypeId, {lrate, Fraction}).

discount(GameTypeId) -> call(GameTypeId, {discount}).

discount(GameTypeId, Fraction)  when is_number(Fraction) -> call(GameTypeId, {discount, Fraction}).

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

handle_info({notify_bot_req, _, _, _, _, _, _, _}, #state{lrate=0} = State) ->
    {noreply, State};
handle_info({notify_bot_req, _GameId, Ialiases, Space, finished, Naliases, Nscores, Moves}
            , #state{ width=Width, height=Height, gravity=Gravity, periodic=Periodic
            , table=Table, lrate=LearningRate, discount=Discount} = State) ->
    train_game(egambo_tictac:samples(Space, Ialiases, Moves, Naliases, Nscores), Table, Width, Height, Gravity, Periodic, LearningRate, Discount, undefined),
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
handle_call({lrate}, _From, State) ->
    {reply, State#state.lrate, State};
handle_call({lrate, Fraction}, _From, State) ->
    {reply, ok, State#state{lrate=Fraction}};
handle_call({discount}, _From, State) ->
    {reply, State#state.discount, State};
handle_call({discount, Fraction}, _From, State) ->
    {reply, ok, State#state{discount=Fraction}};
handle_call(state, _From, State) ->
    {reply, State, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.


%% Scan the moves of a game from last to first and update an aggregated Q-learning table which can also be used to train an ANN (for interpolation)
train_game([], _Table, _Width, _Height, _Gravity, _Periodic, _LearningRate, _Discount, _Qmax) -> ok;
train_game([Sample|Samples], Table, Width, Height, Gravity, Periodic, LearningRate, Discount, Qmax) ->
    NextQmax = train_move(Sample, Table, Width, Height, Gravity, Periodic, LearningRate, Discount, Qmax),
    train_game(Samples, Table, Width, Height, Gravity, Periodic, LearningRate, Discount, NextQmax).

%% Format a single (already Alias-normalized) move for ann use (Board size input vector, Board size / Board Width output vector)
%% and aggregate into the training Q table
%% Score is the final score of the game as seen by the current player
%% Qmax is the updated position quality (with respect to a win of X) for the next move (played after the current one)
%% Qmax is calculated from all Q(Board, Moves) as seen in the perspective of the opponent
%% The Q-learning back-propagation needs to negate this Q for the current player (neglect Joker subtleties for now)
-spec train_move(list(), atom(), integer(), integer(), boolean(), boolean(), number(), number(), egTicTacQlActionQuality()) -> egTicTacQlActionQuality().
train_move([_Board, _Players, _Move, [Score|_], 0], _Table, _Width, _Height, _, _Periodic, _LearningRate, _Discount, undefined) ->
    % ?Info("ql train_move Board ~p and Move ~p for temporary X", [_Board, _Move]),
    % ?Info("ql train_move immediate reward ~p ", [Score]),
    % Do not record this pre-final board state. Decision will be taken by a one step look ahead, not from the Q-Table
    {1, 1, Score}; % (Qmax of final move, +1 for a win or 0 for a tie) % ?QL_AQ_INIT Type
train_move([Board, _Players, Move, _, MTE], Table, Width, Height, false, Periodic, LearningRate, Discount, Qmax) ->
    % ?Info("ql train_move Board ~p and Move ~p for temporary X", [Board, Move]),
    % ?Info("ql train_move target position qmax ~p ", [Qmax]),
    {Input, Sym} = egambo_tictac_sym:norm(Width, Height, false, Periodic, Board),
    % ?Info("ql train_move Input ~p and Sym ~p", [Input, Sym]),
    Olen = Width*Height,            % length of output vector (possible actions)
    OMove = egambo_tictac_sym:map_move(Width, Height, Move, Sym) + 1,   % one based index of move
    train(Input, MTE, Table, OMove, Olen, LearningRate, Discount, Qmax);
train_move([Board, _Players, Move, _, MTE], Table, Width, Height, true, Periodic, LearningRate, Discount, Qmax) ->
    % ?Info("ql train_move Board ~p and Move ~p for temporary X", [Board, Move]),
    % ?Info("ql train_move target position qmax ~p ", [Qmax]),
    {Input, Sym} = egambo_tictac_sym:norm(Width, Height, true, Periodic, Board),
    % ?Info("ql train_move Input and Sym ~p ~p", [Input, Sym]),
    Olen = Width,                   % length of output vector (possible actions)
    OMove = egambo_tictac_sym:map_move(Width, Height, Move rem Width, Sym) + 1,    % one based index to Output
    train(Input, MTE, Table, OMove, Olen, LearningRate, Discount, Qmax).

-spec train(list(), integer(), atom(), integer(), integer(), number(), number(), egTicTacQlActionQuality()) -> egTicTacQlActionQuality().
train(Input, MTE, Table, OMove, Olen, LearningRate, Discount, Qmax) when MTE>0 ->
    {PreviousSampleCount, AggregatedActionQualities} = case imem_meta:read(Table, list_to_binary(Input)) of
        [] ->   % TrainingRec does not exist yet. Cheat a bit, double count the first result.
            {0, qlearn_add(empty_action_qualities(Input, Olen), Qmax, OMove, Olen, LearningRate, Discount)}; 
        [#egTicTacQlSample{nos=N, aaq=OldOut}] ->
            {N, qlearn_add(OldOut, Qmax, OMove, Olen, LearningRate, Discount)}
    end,
    {_, SumN, SumQ} = NewQmax = qlearn_max(AggregatedActionQualities),   % ?QL_AQ_INIT Type
    TrainingRec=#egTicTacQlSample{input=list_to_binary(Input), nos=PreviousSampleCount+1, nmax=SumN, qmax=SumQ/SumN, aaq=AggregatedActionQualities},
    imem_meta:write(Table, TrainingRec),
    NewQmax.

-spec empty_action_qualities(list(), integer()) -> [egTicTacQlActionQuality()].
empty_action_qualities(Input, Olen) ->
    {TruncInput,_} = lists:split(Olen,Input),
    [ if 
        Inp==32 ->   ?QL_AQ_INIT;   % {N, sum(N), sum(Q)} for legal action
        true ->      0              % illegal move (occupied)
      end
    || Inp <- TruncInput 
    ].

-spec qlearn_add([egTicTacQlActionQuality()], egTicTacQlActionQuality(), integer(), integer(), number(), number()) -> [egTicTacQlActionQuality()].
qlearn_add(OldOut, Qmax, OMove, Olen, LearningRate, Discount) ->
    [qlearn_add_one(Old, Pos, Qmax, OMove, LearningRate, Discount) || {Old, Pos} <- lists:zip(OldOut, lists:seq(1, Olen))].

%% Q-learn with immediate reward (Q=0 for TicTacChallenge) from next move's Qmax backpropagation
%% Add one sample to the Q-learning accumulator (single output vector position)
%% accumulated Q:                   {AccN,AccQ}={Number of accumulated samples so far, sum of quality for this action}
%% immediate reward:                {N,Q}       (normally {1,0} for non-final moves, {0,0} for unsampled actions)
%% invalid actions:                 0 + _ -> 0
-spec qlearn_add_one(egTicTacQlActionQuality(), integer(), egTicTacQlActionQuality(), integer(), number(), number()) -> egTicTacQlActionQuality().
qlearn_add_one(?QL_AQ_INVALID, _, _, _, _, _) -> ?QL_AQ_INVALID;    % invalid action 
qlearn_add_one(Qact, Pos, _, OMove, _, _) when Pos/=OMove -> Qact;  % unplayed action
qlearn_add_one({N, _, _}, _, {_, NextN, NextQ}, _, LR, Disc) when LR=:=1,is_integer(Disc) ->   % ?QL_AQ_INIT Types
    {N+1, NextN+Disc, -NextQ};                                      % inverse integer LR=:=1 -> 100% backpropagation
qlearn_add_one({N, AccN, AccQ}, _, {_, NextN, NextQ}, _, LR, Disc) when is_integer(LR),is_integer(Disc) -> 
    {N+1, (LR-1)*AccN+NextN+Disc, (LR-1)*AccQ-NextQ};               % inverse integer e.g. LR=:=2 -> 50% backpropagation
qlearn_add_one(?QL_AQ_INIT, _, {_, NextN, NextQ}, _, LR, Disc) ->   % first sample for this action
    {1, NextN, -LR*Disc*NextQ};
qlearn_add_one({N, AccN, AccQ}, _, {_, NextN, NextQ}, _, LR, Disc) ->   % ?QL_AQ_INIT Types
    AccNN = AccN+1,
    AccQN = AccNN*AccQ/AccN,
    NewAccQ = (1-LR)*AccQN - LR*Disc*NextQ/NextN*AccNN,     % LR=:=0.5 -> 50% backpropagation
    {N+1, AccNN, NewAccQ}.

-spec qlearn_max([egTicTacQlActionQuality()]) -> egTicTacQlActionQuality().
qlearn_max(Output) -> qlearn_max(Output,0,-1.0e200).

-spec qlearn_max([egTicTacQlActionQuality()], egTicTacQlActionQuality(), number()) -> egTicTacQlActionQuality().
qlearn_max([], Max, _) -> Max;  
qlearn_max([?QL_AQ_INVALID|Output], Max, MaxQ) -> qlearn_max(Output, Max, MaxQ);    % invalid action
qlearn_max([{0,_,_}|Output], Max, MaxQ) -> qlearn_max(Output, Max, MaxQ);           % unsampled action ?QL_AQ_INIT Types
qlearn_max([{N,SN,SQ}|Output], _Max, MaxQ) when SQ/SN>MaxQ -> qlearn_max(Output, {N,SN,SQ}, SQ/SN);       % bigger
qlearn_max([{N,SN,SQ}|Output], {_,NMax,_}, MaxQ) when SQ/SN==MaxQ, SN>NMax -> qlearn_max(Output, {N,SN,SQ}, SQ/SN);    % more statistics
qlearn_max([_|Output], Max, MaxQ) -> qlearn_max(Output, Max, MaxQ).                 % smaller or equal

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


-spec play_bot_impl(binary(), integer(), integer(), integer(), boolean(), boolean(), [egAlias()], [egAlias()], float(), float(), atom()) -> egBotMove() | {error, atom()}.
play_bot_impl(Board, Width, Height, _Run, Gravity, Periodic, IAliases, NAliases, Explore, Flatten, Table) ->
    % ?Info("play_bot_impl Board, NAliases ~p ~p", [Board, NAliases]),
    ABoard = egambo_tictac:norm_aliases(Board, NAliases, IAliases),
    % ?Info("play_bot_impl ABoard after alias transform ~p ", [ABoard]),
    {Input, Sym} = egambo_tictac_sym:norm(Width, Height, Gravity, Periodic, ABoard), 
    % ?Info("play_bot_impl Input (normalized board) and symmetry used ~p ~p", [Input, Sym]),
    {NOS, AccActionQuality} = case imem_meta:read(Table, list_to_binary(Input)) of
        [] when Gravity==false ->                   {0, empty_action_qualities(Input, Width*Height)};
        [] ->                                       {0, empty_action_qualities(Input, Width)};
        [#egTicTacQlSample{nos=Nos, aaq=Aaq}] ->    {Nos,Aaq}
    end,
    % ?Info("play_bot_impl ActionQuality ~p",[ActionQuality]),    
    NormIdx = pick(NOS, AccActionQuality, 1, Explore, Flatten),      % 0-based index for predicted move
    NewIdx = egambo_tictac_sym:unmap_move(Width, Height, NormIdx, Sym),    
    % ?Info("play_bot_impl NormIdx, NewIdx (output picked) ~p ~p", [NormIdx, NewIdx]),
    {ok, Idx, NewBoard} = egambo_tictac:put(Gravity, Board, Width, NewIdx, hd(NAliases)),
    % ?Info("play_bot_impl Idx, NewBoard ~p ~p", [Idx, binary_to_list(NewBoard)]),
    {ok, Idx, NewBoard}.

pick(NOS, AccActionQuality, Prior, Explore, 0) ->
    AL = [{ucb1_target(A, NOS, Prior, Explore), rand:uniform(), I} || {A,I} <- lists:zip(AccActionQuality, lists:seq(0, length(AccActionQuality)-1))],
    element(3, lists:last(lists:sort(AL)));
pick(NOS, AccActionQuality, Prior, Explore, Flatten) ->
    AL = [{ucb1_target(A, NOS, Prior, Explore) + Flatten*rand:uniform(), I} || {A,I} <- lists:zip(AccActionQuality, lists:seq(0, length(AccActionQuality)-1))],
    element(2, lists:last(lists:sort(AL))).

-spec ucb1_target(egTicTacQlActionQuality(), integer(), number(), number()) -> number().
ucb1_target(?QL_AQ_INVALID, _, _, _) -> -100; 
ucb1_target(?QL_AQ_INIT, _, _, _) -> 100; 
ucb1_target({N,SN,SQ}, NOS, _Prior, Explore) ->    % ?QL_AQ_INIT Type
    SQ/SN + ?UCB1_BONUS(NOS, N, _Prior, Explore).

% ucb1_target({N,Q}, NOS, Prior, Explore) -> 
%     DE = 1.0 + Explore*math:sqrt(NOS)/(1+N),
%     (Q/N + DE) / DE.
% ucb1_target({N,Q}, NOS, Explore) -> Q/N + Explore*math:sqrt(NOS)/(1+N).

