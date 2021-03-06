-module(egambo_tictac_ann).

-include("egambo_tictac.hrl").  % import tictac game definitions 

-behavior(gen_server).          % callbacks provided by (bot) players
%-behavior(egambo_gen_player).  % callbacks provided by (bot) players

-define(ANN_MODEL, egTicTacAnnModel).       % Model Table Name (Layers, Weights)
-define(ANN_TRAIN, egTicTacAnnTrain).       % Training Table Name
-define(ANN_TEST,  egTicTacAnnTest).        % Test Table Name

-define(ANN_MODEL_OPTS, [{record_name, egTicTacAnnModel}]).        
-define(ANN_TRAIN_OPTS, [{record_name, egTicTacAnnSample}, {type, ordered_set}]).        
-define(ANN_TEST_OPTS,  [{record_name, egTicTacAnnSample}, {type, ordered_set}]).        

-define(ANN_HIDDEN_BREATH, 2.0).    % Number of neurons in hidden layer = ROUND(board size * ?ANN_HIDDEN_BREATH)
-define(ANN_EXTRA_LAYERS, 0).       % Number of total layers = board width + ?ANN_EXTRA_LAYERS (inluding Input and Output Layers)
-define(ANN_ACTIVATION, tanh1).     % tanh1, tanh, relu or sigmoid
-define(ANN_OUTPUT_TARGET, 1.0).    % Output swing of 'most important move' (should correlate to point of max. nonlinearity of AF)
-define(ANN_OUTPUT_MTE, 4.0).       % Optimize for sensitivity for move 4 before the end of the game (4 more half-moves to end)
-define(ANN_OUTPUT_UNSAMPLED, 0.001).       % Value emitted as output for unsampled move in Export
-define(ANN_HASH_BASE, 4294967296). 
-define(ANN_QLEARN_ALFA, 0.9). 
-define(ANN_QLEARN_GAMMA, -0.9).    % Next move (action) is from opponent. Reward Q(s,a) needs to be negated 


-define(ANN_NO_TEST_SAMPLES, {error, <<"No test samples found">>}).
-define(ANN_BAD_OUTPUT, {error, <<"Bad Network Output">>}).
-define(ANN_NOT_LEARNING, {error, <<"Network is not in learning mode">>}).
-define(ANN_NOT_PLAYING, {error, <<"Network is not in playing mode">>}).
-define(ANN_BAD_RATE, {error, <<"Invalid learning rate for training">>}).


-record(egTicTacAnnModel,   { tid               :: egGameTypeId()
                            , version = <<>>    :: binary()    % network version name
                            , info     = <<>>   :: binary()    % network description / res
                            , time = undefined  :: ddTimeUID() % save time
                            , layers = []       :: list()      % number of layers and neurons per layer
                            , weights = []      :: list()      % neuron weights (including bias neurons)
                            }).

-define(egTicTacAnnModel,   [ binstr
                            , binstr
                            , binstr
                            , timestamp
                            , list
                            , list
                            ]).
% rd(egTicTacAnnModel, {id=, version=, info=, time= layers=, weights=}).

-record(egTicTacAnnSample,  { skey= {<<>>, 0}   :: {egGameTypeId(),any()}
                            , qos=1             :: number()   % quality of sample (-1..+1)
                            , input=[]          :: list()     % sample network input
                            , output=[]         :: list()     % expected network output
                            }).

-define(egTicTacAnnSample,   [ tuple
                            , number
                            , list
                            , list
                            ]).
% rd(egTicTacAnnSample, {skey=, qos=, input=, output=}).

-record(state,  { bid        :: egBotId()       % bot id (= module name)
                , tid        :: egGameTypeId()  % game type id
                , engine     :: egEngine()      % game engine (rule engine)
                , width=3    :: integer()       % board width >= 3
                , height=3   :: integer()       % board height >= 3
                , run=3      :: integer()       % sucess run length
                , gravity=false :: boolean()    % do moves fall towards higher row numbers
                , periodic=false :: boolean()   % unbounded repeating board
                , ialiases   :: [egAlias()]     % normalized player order
                , winmod     :: egWinId()       % win function module
                , players=2  :: integer()       % number of players 
                , status     :: egBotStatus()   % current bot status (e.g. learning / playing)
                , layers=[]  :: [integer()]     % neuron count per  layer
                , explore=0.0 :: float()        % rate for random exploration (0..1.0)
                , flatten=0.0 :: float()        % relative output threshold for flat sampling (random picking)
                , rate=0.05  :: float()         % default learning rate
                , epochs=10  :: integer()       % default learning epochs
                , error=0.001 :: float()        % default learning error
                , network  :: undefined | pid() % network root process
                , version= <<>> :: binary()       % network version name
                , info= <<>> :: binary()       % network info
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
        , train/7
        , test/3
        , save/1
        , save/2
        , save/3
        , ann_samples/7
        , ann_samples/9
        , read_samples/6
        , predict/2
        , pick_output/3
        , norm/1
        , explore/1
        , explore/2
        , flatten/1
        , flatten/2
        ]).

-export([ ann_norm_input/1
        , ann_norm_sample_output/2
        , qlearn_max/1
        ]).

% egambo_tictac_ann:resume(<<"tic_tac_toe">>).
% egambo_tictac_ann:start_learning(<<"tic_tac_toe_443">>).
% egambo_tictac_ann:train(<<"tic_tac_toe_443">>, 20    , 0.1   , 0.10, 0.0  , 1     , 150).
% egambo_tictac_ann:train(GameTypeId           , Epochs, ResErr, Rate, QosMin, QosMax, BatchSize) 
% egambo_tictac_ann:save(<<"tic_tac_toe_443">> , <<"V0.1">>,<<"First Attempt for Learning">>). 
% egambo_tictac_ann:stop(<<"tic_tac_toe_443">>).
% egambo_tictac_ann:predict(<<"tic_tac_toe">>,[0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]).

-define(BOT_IS_BUSY, {error, bot_is_busy}).

-safe([state, start, stop, resume, save, start_learning, start_playing, norm, ann_samples, train, test, ann_norm_input, ann_norm_sample_output, explore, flatten, predict]).

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
    case supervisor:start_child(egambo_bot_sup, ChildSpec) of
        {ok,_} ->                       ok;
        {ok,_,_} ->                     ok;
        {error, already_present} ->     ok;
        {error,{already_started,_}} ->  ok;
        Error ->                        Error
    end.

train(GameTypeId, Epochs, ResErr, Rate, QosMin, QosMax, BatchSize) ->
    train(GameTypeId, Epochs, ResErr, Rate, QosMin, QosMax, BatchSize, -1.0e100, 1).

train(GameTypeId, Epochs, ResErr, Rate, QosMin, QosMax, BatchSize, LastKey, BatchId) when Rate > 0.0, Rate =< 1.0 -> 
    case read_samples(?ANN_TRAIN, GameTypeId, LastKey, BatchSize, QosMin, QosMax) of
        {[], true} -> 
            ok;
        {L, true} ->
            ?Info("Training ann for batch ~p of length ~p and game type ~s", [BatchId, length(L), GameTypeId]),
            TrainingSet = [ {ann_norm_input(I), ann_norm_sample_output(O, QOS)} || [_Key, QOS, I, O] <- L],
            % ?Info("First Sample ~p", [hd(TrainingSet)]),
            % ?Info("Last Sample ~p", [lists:last(TrainingSet)]),
            gen_server:call(?BOT_GID(?MODULE, GameTypeId), {train, Epochs, ResErr, Rate, TrainingSet}, infinity);
        {L, false} ->
            ?Info("Training ann for batch ~p of length ~p and game type ~s", [BatchId, length(L), GameTypeId]),
            TrainingSet = [ {ann_norm_input(I), ann_norm_sample_output(O, QOS)} || [_Key, QOS, I, O] <- L],
            % ?Info("First Sample ~p", [hd(TrainingSet)]),
            % ?Info("Last Sample ~p", [lists:last(TrainingSet)]),
            case gen_server:call(?BOT_GID(?MODULE, GameTypeId), {train, Epochs, ResErr, Rate, TrainingSet}, infinity) of
                Err when is_tuple(Err) ->  Err;
                ok -> train(GameTypeId, Epochs, ResErr, Rate, QosMin, QosMax, BatchSize, element(2, hd(lists:last(L))), BatchId+1)
            end
    end;
train(_, _, _, _, _, _, _, _, _) ->  ?ANN_BAD_RATE.

test(GameTypeId, QosMin, QosMax) ->
    case read_samples(?ANN_TEST, GameTypeId, -1.0e100, 1000000, QosMin, QosMax) of
        [[], true] -> 
            ?ANN_NO_TEST_SAMPLES;
        [L, _] ->
            TestSet = [ {ann_norm_input(I), ann_norm_sample_output(O, QOS)} || {_Key, QOS, I, O} <- L],
            gen_server:call(?BOT_GID(?MODULE, GameTypeId), {test, TestSet}, infinity)
    end.

predict(GameTypeId, Input) ->
    gen_server:call(?BOT_GID(?MODULE, GameTypeId), {predict, Input}).

explore(GameTypeId) ->
    gen_server:call(?BOT_GID(?MODULE, GameTypeId), {explore}).

explore(GameTypeId, Fraction) when is_number(Fraction)->
    gen_server:call(?BOT_GID(?MODULE, GameTypeId), {explore, Fraction}).

flatten(GameTypeId) ->
    gen_server:call(?BOT_GID(?MODULE, GameTypeId), {flatten}).

flatten(GameTypeId, Fraction)  when is_number(Fraction) ->
    gen_server:call(?BOT_GID(?MODULE, GameTypeId), {flatten, Fraction}).

-spec stop(egBotId()) -> ok | egGameError().
stop(GameTypeId) ->
    gen_server:call(?BOT_GID(?MODULE, GameTypeId), finish), 
    supervisor:terminate_child(egambo_bot_sup, ?BOT_ID(?MODULE, GameTypeId)),
    supervisor:delete_child(egambo_bot_sup, ?BOT_ID(?MODULE, GameTypeId)).

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

read_samples(TableName, GameTypeId, LastKey, Limit, QosMin, QosMax) ->
    MatchCond = [{'>', '$1', match_val({GameTypeId, LastKey})}
                ,{'>', '$2', QosMin}
                ,{'=<', '$2', QosMax}
                ], 
    MatchFunction = {#egTicTacAnnSample{skey='$1', qos='$2', input='$3', output='$4'}, MatchCond, ['$$']},
    imem_meta:select(TableName, [MatchFunction], Limit).

match_val(T) when is_tuple(T) -> {const,T};
match_val(V) -> V.


%% Sample all moves of a game and aggregate Q-learned winning chances into the training table
%% Key in the training table is a hash of the (alias normalized and symmetry normalized) board atate before the move (action) 
%% This follows an adapted the Q-learnng idea where the winning estimate of the current {position, action}
%% is calculated as the negated (maximum) winning chance of the opponent in that position  
ann_samples(GameTypeId, _GameId, Space, Ialiases, Moves, Naliases, Nscores) ->
    ann_samples(GameTypeId, _GameId, Space, Ialiases, Moves, Naliases, Nscores, ?ANN_QLEARN_ALFA, ?ANN_QLEARN_GAMMA).

ann_samples(GameTypeId, _GameId, Space, Ialiases, Moves, Naliases, Nscores, Alfa, Gamma) ->
    #egGameType{params=Params} = egambo_game:read_type(GameTypeId),
    ann_samples_game(GameTypeId, egambo_tictac:samples(Space, Ialiases, Moves, Naliases, Nscores), Params, Alfa, Gamma, undefined,[]).

%% Scan the moves of a game from last to first and update an aggregated Q-learning table which can also be used to train an ANN (for interpolation)
ann_samples_game(_, [], _, _, _, _, Acc) -> lists:reverse(Acc);
ann_samples_game(GameTypeId, [Sample|Samples], Params, Alfa, Gamma, LastTrainingRec, Acc) ->
    {Hash, Output, TrainingRec} = ann_sample_single(GameTypeId, Sample, Params, Alfa, Gamma, LastTrainingRec),
    ann_samples_game(GameTypeId, Samples, Params, Alfa, Gamma, TrainingRec, [{Hash, Output}|Acc]).

%% Format a single (already Alias-normalized) sample for ann use (Board size input vector, Board size / Board Width output vector)
%% and aggregate into the training table using an input hash
%% Score is the final score of the game as seen by the current player
%% LastTrainingRec is the updated training data for the next move (played after the current one)
%% LastTrainingRec is containing all Q(Board,Moves) as seen in the perspective of the opponent
%% The Q-learning back-propagation needs to negate this Q for the current player
%% The estimate for the quality of this move hence is the negated current Q of the LastTrainingRec  
ann_sample_single(GameTypeId, [Board, _Players, Move, [Score|_], MTE], Params, Alfa, Gamma, LastTrainingRec) ->
    #{width:=Width, height:=Height, gravity:=Gravity, periodic:=Periodic} = Params,
    % ?Info("ann_sample_single Board and Move ~p ~p", [Board, Move]),
    {Input, Sym} = egambo_tictac_sym:norm(Width, Height, Gravity, Periodic, Board),
    % ?Info("ann_sample_single Input and Sym ~p ~p", [Input, Sym]),
    Q = case MTE of
        0 -> Score;
        _ -> 0
    end,
    {Output, OMove, Olen} = case Gravity of
        false ->
            OL = Width*Height,
            OMove1 = egambo_tictac_sym:map_move(Width, Height, Move, Sym) + 1,              % one based index of move
            F = fun(I) -> 
                Inp=lists:nth(I, Input), 
                if 
                    I==OMove1 -> {1, Q};      % move taken in sample
                    Inp==32 ->   {0, 0};      % alternative legal move, not taken here
                    true ->      0            % illegal move (occupied)
                end 
            end, 
            {lists:map(F, lists:seq(1, OL)), OMove1, OL};
        true ->
            OL = Width,
            OMove1 = egambo_tictac_sym:map_move(Width, Height, Move rem Width, Sym) + 1,    % one based index to Output
            F = fun(I) -> 
                Inp=lists:nth(I, Input), 
                if 
                    I==OMove1 -> {1, Q};      % move taken in sample
                    Inp==32 ->   {0, 0};      % alternative legal move, not taken here
                    true ->      0            % illegal move (occupied)
                end 
            end, 
            {lists:map(F, lists:seq(1, OL)), OMove1, OL}
    end,
    % ?Info("ann_sample_single Output ~p OutputIdx ~p", [Output, OMove]),
    Hash = erlang:phash2(Input, ?ANN_HASH_BASE),
    case ann_sample_training(Input, Output, MTE, OMove, Olen, Alfa, Gamma, {GameTypeId, Hash}, LastTrainingRec) of
        hash_collision ->       % Hash is already used for another board
            case ann_sample_training(Input, Output, MTE, OMove, Olen, Alfa, Gamma, {GameTypeId, -Hash}, LastTrainingRec) of    
                hash_collision ->   % Hash is already used for another board
                    ?Info("Unrecoverable input hash collision for Hash=~p Input=~p, sample ignored", [Hash, Input]),
                    {0, Output, undefined};
                AltTrainingRec ->
                    imem_meta:write(?ANN_TRAIN, AltTrainingRec),
                    {-Hash, Output, AltTrainingRec}
            end;
        TrainingRec ->
            imem_meta:write(?ANN_TRAIN, TrainingRec),
            {Hash, Output, TrainingRec}
    end.

ann_sample_training(Input, Output, 0, _OMove, _Olen, _Alfa, _Gamma, Key, undefined) ->
    case imem_meta:read(?ANN_TRAIN, Key) of
        [] ->
            % TrainingRec does not exist yet, create it   
            #egTicTacAnnSample{skey=Key, qos=1, input=Input, output=Output};
        [#egTicTacAnnSample{qos=N, input=Input, output=OldOut}] ->
            % Immediate win or tie, cannot expect next Q there. Record the observed Score by adding to the accu
            #egTicTacAnnSample{skey=Key, qos=N+1, input=Input, output=vector_add(OldOut, Output)};
        _ ->
            hash_collision
    end;
ann_sample_training(Input, Output, _MTE, _OMove, _Olen, _Alfa, _Gamma, Key, undefined)  ->
    case imem_meta:read(?ANN_TRAIN, Key) of
        [] ->
            % TrainingRec does not exist yet, create it   
            #egTicTacAnnSample{skey=Key, qos=1, input=Input, output=Output};
        [#egTicTacAnnSample{qos=N, input=Input, output=OldOut}] ->
            % Not the last move but cannot Q-learn from next action (record just processed is lost due to hash collision)
            #egTicTacAnnSample{skey=Key, qos=N+1, input=Input, output=vector_add(OldOut, Output)}
    end;
ann_sample_training(Input, Output, MTE, OMove, Olen, Alfa, Gamma, Key, LastTrainingRec) when MTE>0 ->
    case imem_meta:read(?ANN_TRAIN, Key) of
        [] ->
            % TrainingRec does not exist yet, create it   
            #egTicTacAnnSample{output=NextOut} = LastTrainingRec,
            % ?Info("LastTrainingRec ~p",[LastTrainingRec]),
            #egTicTacAnnSample{skey=Key, qos=1, input=Input, output=qlearn_add(Output, Output, NextOut, OMove, Olen, Alfa, Gamma)};
        [#egTicTacAnnSample{qos=N, input=Input, output=OldOut}] ->
            % Not the last move, Q-learn from next action (just processed)
            #egTicTacAnnSample{output=NextOut} = LastTrainingRec,
            % ?Info("LastTrainingRec ~p",[LastTrainingRec]),
            #egTicTacAnnSample{skey=Key, qos=N+1, input=Input, output=qlearn_add(OldOut, Output, NextOut, OMove, Olen, Alfa, Gamma)};
        _ ->
            hash_collision
    end.


%% Output (gain) of a single sample output item depending on the MTE (moves to end)
%% An MTE of 0 (a win) is placed somewhat above the higest sensitivity of the AF
%% An MTE of 1 (a loose/tie) is placed somewhat below higest sensitivity in the negative
%% A (roughly) symmetric AF around the origin is assumed here
% ann_sample_gain(Width, Height, Score, MTE) when Score >= 0.0 ->
%     ?ANN_OUTPUT_TARGET * Score * (Width*Height-MTE) / (Width*Height-?ANN_OUTPUT_MTE);
% ann_sample_gain(Width, Height, Score, MTE) ->
%     ?ANN_OUTPUT_TARGET * Score * (Width*Height-MTE+1) / (Width*Height-?ANN_OUTPUT_MTE).

vector_add(OldOut, Output) ->
    [vector_add_one(Old, Out) || {Old, Out} <- lists:zip(OldOut, Output)].

vector_add_one(0, 0) -> 0;
vector_add_one({AccN,AccQ}, {N,Q}) -> {AccN+N, AccQ+Q}. 

qlearn_add(OldOut, Output, NextOut, OMove, Olen, Alfa, Gamma) ->
    [qlearn_add_one(Old, Out, Pos, NextOut, OMove, Alfa, Gamma) || {Old, Out, Pos} <- lists:zip3(OldOut, Output, lists:seq(1, Olen))].


%% Add one sample to the Q-learning accumulator (single output vector position)
%% accumulated Q:                   {AccN,AccQ}={Number of accumulated samples so far, sum of quality for this action}
%% immediate reward:                {N,Q}       (normally {1,0} for non-final moves, {0,0} for unsampled actions)
%% invalid moves (actions):         0 + 0 -> 0
qlearn_add_one(0, 0, _, _, _, _, _) -> 0;                           % invalid moves 
qlearn_add_one({AccN,AccQ}, {0,_}, _, _, _, _, _) -> {AccN,AccQ};   % unsampled actions
qlearn_add_one({0,0}, {N,Q}, OMove, NextOut, OMove, Alfa, Gamma) ->
    % Q-learn with immediate reward (Q=0 for TicTacChallenge)
    {CN,CQ} = qlearn_max(NextOut),
    {N, Alfa*(Q + Gamma*N*CQ/CN)};
qlearn_add_one({AccN,AccQ}, {N,Q}, OMove, NextOut, OMove, Alfa, Gamma) ->
    % Q-learn with immediate reward (Q=0 for TicTacChallenge)
    {CN,CQ} = qlearn_max(NextOut),
    AccNN = AccN+N,
    AccQN = AccNN*AccQ/AccN,
    {AccNN, AccQN + Alfa*(Q*AccNN/N + Gamma*CQ*AccNN/CN - AccQN)}.

qlearn_max(Output) -> qlearn_max(Output,0,-1.0e100).

qlearn_max([], Max, _) -> Max;  
qlearn_max([0|Output], Max, MaxQ) -> qlearn_max(Output, Max, MaxQ);     % invalid action
qlearn_max([{0,_}|Output], Max, MaxQ) -> qlearn_max(Output, Max, MaxQ); % unsampled action
qlearn_max([{N,Q}|Output], _Max, MaxQ) when Q/N>MaxQ -> qlearn_max(Output, {N,Q}, Q/N); % bigger
qlearn_max([{N,Q}|Output], {NMax,_}, MaxQ) when Q/N==MaxQ; N>NMax -> qlearn_max(Output, {N,Q}, Q/N); % more statistics
qlearn_max([{_,_}|Output], Max, MaxQ) -> qlearn_max(Output, Max, MaxQ). % smaller or equal

ann_norm_input(L) -> [ann_norm_single_input(I) || I <- L].

ann_norm_single_input(32) -> 0.0;
ann_norm_single_input($X) -> 1.0;
ann_norm_single_input($O) -> -1.0;
ann_norm_single_input($*) -> 0.5;
ann_norm_single_input($$) -> -0.5;
ann_norm_single_input(I) -> I/256.0.

ann_norm_sample_output(AggregatedOutputVector, _QOS) ->
    Fun = fun(0) -> 0; ({0,_}) -> ?ANN_OUTPUT_UNSAMPLED;  ({N,S}) -> S/N end,     % free floating for occupied
    lists:map(Fun, AggregatedOutputVector). 

norm(Vector) -> 
    Fun = fun(W, Acc) -> W*W+Acc end,
    math:sqrt(lists:foldl(Fun, 0.0, Vector)).

init([GameTypeId]) ->
    Result = try
        imem_meta:init_create_table(?ANN_MODEL, {record_info(fields, egTicTacAnnModel), ?egTicTacAnnModel, #egTicTacAnnModel{}}, ?ANN_MODEL_OPTS, system),  
        imem_meta:init_create_table(?ANN_TRAIN, {record_info(fields, egTicTacAnnSample), ?egTicTacAnnSample, #egTicTacAnnSample{}}, ?ANN_TRAIN_OPTS, system),  
        imem_meta:init_create_table(?ANN_TEST,  {record_info(fields, egTicTacAnnSample), ?egTicTacAnnSample, #egTicTacAnnSample{}}, ?ANN_TEST_OPTS, system),  
        #egGameType{engine=Engine, players=Players, params=Params} = egambo_game:read_type(GameTypeId),
        Width=maps:get(width, Params),
        Height=maps:get(height, Params),
        Run=maps:get(run, Params),
        Gravity=maps:get(gravity, Params),
        Periodic=maps:get(periodic, Params),
        Aliases=maps:get(aliases, Params), 
        {Layers, Network, Status, Version, Info} = case imem_meta:read(?ANN_MODEL, GameTypeId) of
            [] ->
                L = layer_default(Width, Height, Run, Gravity, Periodic),
                ?Info("Creating random Layers ~p", [L]),  
                {L, ann:create_neural_network(L, ?ANN_ACTIVATION), learning, <<>>, <<>>};
            [#egTicTacAnnModel{layers=L, weights=W, version=V, info=I}] ->
                {L, ann:create_neural_network(L, lists:flatten(W), ?ANN_ACTIVATION), playing, V, I}
        end,
        State = #state{ bid=?MODULE 
                      , tid=GameTypeId
                      , engine=Engine
                      , width=Width
                      , height=Height
                      , run=Run
                      , gravity=Gravity
                      , periodic=Periodic
                      , ialiases=Aliases
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

layer_default(3=Width, Height, _Run, _Gravity, _Periodic) ->
    [Width*Height|[Width*Height || _ <- lists:seq(1, Width)]];
layer_default(Width, Height, _Run, _Gravity, _Periodic) ->
    [Width*Height|[round(Width*Height*?ANN_HIDDEN_BREATH) || _ <- lists:seq(3, Width + ?ANN_EXTRA_LAYERS)]] ++ [Width*Height].

start_link(GameTypeId)  ->
    gen_server:start_link(?BOT_GID(?MODULE, GameTypeId), ?MODULE, [GameTypeId], []).

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_cast({play_bot_req, GameId, _, [Player|_]}, #state{status=Status} = State) when Status /= playing ->
    play_bot_resp(GameId, Player, ?BOT_NOT_PLAYING),
    {noreply, State};
handle_cast({play_bot_req, GameId, Board, NAliases}, #state{ width=Width, height=Height, run=Run, network=Network
                                                          , gravity=Gravity, periodic=Periodic, winmod=WinMod
                                                          , ialiases=IAliases, explore=Explore, flatten=Flatten
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
                    case rand:uniform() of
                        R when R<Explore ->
                            {ok, Idx, NewBoard} = egambo_tictac_bot:play_bot_random(Board, Width, Height, Run, Gravity, Periodic, WinMod, NAliases, Options),
                            play_bot_resp(GameId, hd(NAliases), {ok, Idx, NewBoard});
                        _ ->
                            {ok, Idx, NewBoard} = play_bot_impl(Board, Width, Height, Run, Gravity, Periodic, IAliases, NAliases, Network, Flatten),
                            play_bot_resp(GameId, hd(NAliases), {ok, Idx, NewBoard})
                    end;
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

handle_info({'EXIT', Network, neural_network_shutdown}, #state{tid=GameTypeId, network=Network, status=playing} = State) -> 
    ?Info("Network stopped in playing state for game type ~p  ~p",[?MODULE, GameTypeId]),
    {noreply, State};
handle_info({'EXIT', Network, neural_network_shutdown}, #state{tid=GameTypeId, network=Network} = State) -> 
    ?Info("Network stopped for game type ~p  ~p",[?MODULE, GameTypeId]),
    {noreply, State};
handle_info(Reqest, State) when element(1, Reqest) == notify_bot_req ->
    % ignore game notifications 
    {noreply, State};
handle_info(Request, State) -> 
    ?Info("Unsolicited handle_info in ~p : ~p",[?MODULE, Request]),
    {noreply, State}.

handle_call(finish, _From, #state{network=Network} = State) ->
    Network ! {finish},
    {reply, ok, State#state{network=undefined}};
handle_call(start_learning, _From, State) ->
    {reply, ok, State#state{status=learning}};
handle_call({predict, Input}, _From, #state{network=Network, status=playing} = State) ->
    Inp = case lists:member(32, Input) of
        false -> Input;
        true ->  ann_norm_input(Input)
    end,
    ?Info("Predict Input ~p",[Inp]),
    Out = ann:predict(Network, Inp),
    ?Info("Predict Output ~p",[Out]),
    {reply, Out, State};
handle_call({predict, _}, _From, State) ->
    {reply, ?ANN_NOT_PLAYING, State};
handle_call({explore}, _From, State) ->
    {reply, State#state.explore, State};
handle_call({explore, Fraction}, _From, State) ->
    {reply, ok, State#state{explore=Fraction}};
handle_call({flatten}, _From, State) ->
    {reply, State#state.flatten, State};
handle_call({flatten, Fraction}, _From, State) ->
    {reply, ok, State#state{flatten=Fraction}};
handle_call(start_playing, _From, State) ->
    {reply, ok, State#state{status=playing}};
handle_call({train, Epochs, ResErr, Rate, TrainingSet}, _From, #state{network=Network, status=learning} = State) ->
    % ?Info("training set ~p at rate ~p", [TrainingSet, Rate]),
    {reply, ann:train(Network, Epochs, ResErr, Rate, TrainingSet), State};
handle_call({test, TestSet}, _From, #state{network=Network, status=learning} = State) ->
    {reply, ann:compute_error(Network, TestSet), State};
handle_call({train, _, _, _, _}, _From, State) ->
    {reply, ?ANN_NOT_LEARNING, State};
handle_call({test, _}, _From, State) ->
    {reply, ?ANN_NOT_LEARNING, State};
handle_call(save, _From, #state{version=Version, info=Info} = State) ->
    handle_call({save, Version, Info}, _From, State);
handle_call({save, Version}, _From, #state{info=Info} = State) ->
    handle_call({save, Version, Info}, _From, State);
handle_call({save, Version, Info}, _From, #state{tid=GameTypeId, layers=Layers, network=Network} = State) ->
    imem_meta:write(?ANN_MODEL, #egTicTacAnnModel{tid=GameTypeId , time=egambo_game:eg_time(), version=Version, info=Info, layers=Layers, weights=ann:get_weights(Network)}),
    {reply, ok, State#state{version=Version, info=Info}};
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

-spec play_bot_impl(binary(), integer(), integer(), integer(), boolean(), boolean(), [egAlias()], [egAlias()], pid(), number()) -> egBotMove().
play_bot_impl(Board, Width, Height, _Run, Gravity, Periodic, IAliases, [Player|_]=NAliases, Network, Flatten) ->
    % ?Info("play_bot_impl Board, NAliases ~p ~p", [Board, NAliases]),
    ABoard = egambo_tictac:norm_aliases(Board, NAliases, IAliases),
    % ?Info("play_bot_impl ABoard after alias transform ~p ", [ABoard]),
    {NormBoard, Sym} = egambo_tictac_sym:norm(Width, Height, Gravity, Periodic, ABoard), 
    % ?Info("play_bot_impl normalized Board and symmetry used ~p ~p", [NormBoard, Sym]),
    NormIn = ann_norm_input(NormBoard),
    % ?Info("play_bot_impl Network, NormIn ~p ~p ", [Network, NormIn]),
    Output = ann:predict(Network, NormIn),
    % ?Info("play_bot_impl network Output ~p",[Output]),    
    NormIdx = pick_output(Output, NormBoard, Flatten),      % 0-based index for predicted move
    NewIdx = egambo_tictac_sym:unmap_move(Width, Height, NormIdx, Sym),    
    % ?Info("play_bot_impl NormIdx, NewIdx (output picked) ~p ~p", [NormIdx, NewIdx]),
    {ok, Idx, NewBoard} = egambo_tictac:put(Gravity, Board, Width, NewIdx, Player),
    % ?Info("play_bot_impl Idx, NewBoard ~p ~p", [Idx, binary_to_list(NewBoard)]),
    {ok, Idx, NewBoard}.

-spec pick_output(list(), list(), number()) -> integer().
pick_output(Output, NormBoard, Flatten) ->
    Valid = fun({_G, I}) -> (lists:nth(I, NormBoard) == ?AVAILABLE) end,
    SortedGainList = lists:sort(lists:filter(Valid, lists:zip(Output, lists:seq(1, length(Output))))),
    {MaxGain, _} = lists:last(SortedGainList),
    Pred = if 
        MaxGain =< 0.0 ->   fun({G,_}) -> (G<MaxGain-1.0e-7) end; 
        true ->             fun({G,_}) -> (G<MaxGain*(1.0-Flatten)-1.0e-7) end
    end,
    ShortList = lists:dropwhile(Pred, SortedGainList),
    element(2, lists:nth(rand:uniform(length(ShortList)), ShortList)) -1.   % 0-based index to one of the top gains

% min_max(L) -> 
%     MinMax = fun(E, {Min,Max}) ->
%         case {(E<Min),(E>Max)} of
%             {true, true} -> {E,E};
%             {false,true} -> {Min,E};
%             {true,false} -> {E,Max};
%             {false,false} -> {Min,Max}
%         end
%     end,
%     lists:foldl(MinMax, {[nil|<<255>>],-1.0e100}, L). 

%% ===================================================================
%% TESTS
%% ===================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

qlearn_max_test_() ->
    W = 3,
    H = 3,
    Board = lists:seq($a,$a+W*H-1),
    [ {"{1,1}",   ?_assertEqual({1,1}, qlearn_max([0,0,{0,0},{1,0},{1,1}]))}
    , {"{1,0}", ?_assertEqual({1,0}, qlearn_max([0,0,{0,0},{1,0},{1,-1}]))}
    , {"{1,-0.5}", ?_assertEqual({1,-0.5}, qlearn_max([0,0,{0,0},{1,-0.5},{1,-1}]))}
    ].

-endif.
