-module(egambo_tictac_ann).

-include("egambo_tictac.hrl").  % import tictac game definitions 

-behavior(gen_server).          % callbacks provided by (bot) players
%-behavior(egambo_gen_player).   % callbacks provided by (bot) players

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
        , ann_sample/7
        , ann_samples/7
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
        ]).

% egambo_tictac_ann:resume(<<"tic_tac_toe_443">>).
% egambo_tictac_ann:start_learning(<<"tic_tac_toe_443">>).
% egambo_tictac_ann:train(<<"tic_tac_toe_443">>, 20    , 0.1   , 0.10, 0.0  , 1     , 150).
% egambo_tictac_ann:train(GameTypeId           , Epochs, ResErr, Rate, QosMin, QosMax, BatchSize) 
% egambo_tictac_ann:save(<<"tic_tac_toe_443">> , <<"V0.1">>,<<"First Attempt for Learning">>). 
% egambo_tictac_ann:stop(<<"tic_tac_toe_443">>).
% egambo_tictac_ann:predict(<<"tic_tac_toe_443">>,"XXO  OO   XO   X").

-define(BOT_IS_BUSY, {error, bot_is_busy}).

-safe([state, start, stop, resume, save, start_learning, start_playing, ann_sample, norm, ann_samples, train, test, ann_norm_input, ann_norm_sample_output, explore, flatten]).

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

predict(GameTypeId, NormBoard) ->
    gen_server:call(?BOT_GID(?MODULE, GameTypeId), {predict, NormBoard}).

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


%% Sample one random move out of a game into the training table
%% and aggregate into the training table using an input hash
ann_sample(GameTypeId, _GameId, Space, Ialiases, Moves, Naliases, Nscores) ->
    [Input, _Players, Move, [Score|_], MTE] = egambo_tictac:sample(Space, Ialiases, Moves, Naliases, Nscores),
    ann_sample_single(GameTypeId, Input, Move, Score, MTE).

%% Sample all moves of a game into the training table (ok to do as long as we have enough ramdomness)
%% and aggregate into the training table using an input hash
ann_samples(GameTypeId, _GameId, Space, Ialiases, Moves, Naliases, Nscores) ->
    [ ann_sample_single(GameTypeId, Input, Move, Score, MTE) || 
        [Input, _Players, Move, [Score|_], MTE] <- egambo_tictac:samples(Space, Ialiases, Moves, Naliases, Nscores)
    ].

%% Format a single (already Alias-normalized) sample for ann use (Board size input vector, Board size / Board Width output vector)
%% and aggregate into the training table using an input hash
ann_sample_single(GameTypeId, Board, Move, Score, MTE) ->
    #egGameType{params=#{width:=Width, height:=Height, gravity:=Gravity, periodic:=Periodic}} = egambo_game:read_type(GameTypeId),
    % ?Info("ann_sample_single Board and Move ~p ~p", [Board, Move]),
    {Input, Sym} = egambo_tictac_sym:norm(Width, Height, Gravity, Periodic, Board),
    % ?Info("ann_sample_single Input and Sym ~p ~p", [Input, Sym]),
    Gain = ann_sample_gain(length(Input), Score, MTE),
    Output = case Gravity of
        false ->
            Move1 = egambo_tictac_sym:map_move(Width, Height, Move, Sym) + 1,  % one based index of move
            F = fun(I) -> 
                Inp=lists:nth(I, Input), 
                if 
                    I==Move1 -> {1, Gain};   % move taken in sample
                    Inp==32 ->  {0, 0};      % alternative legal move, not taken here
                    true ->     0            % illegal move (occupied)
                end 
            end, 
            lists:map(F, lists:seq(1, length(Input)));
        true ->
            Move1 = egambo_tictac_sym:map_move(Width, Height, Move rem Width, Sym) + 1, % one based index of move
            F = fun(I) -> 
                Inp=lists:nth(I, Input), 
                if 
                    I==Move1 -> {1, Gain};   % move taken in sample
                    Inp==32 ->  {0, 0};      % alternative legal move, not taken here
                    true ->     0            % illegal move (occupied)
                end 
            end, 
            lists:map(F, lists:seq(1, Width))
    end,
    % ?Info("ann_sample_single Output ~p", [Output]),
    Hash = erlang:phash2(Input, 4294967296),
    Key = {GameTypeId, Hash},
    case imem_meta:read(?ANN_TRAIN, Key) of
        [] ->   
            imem_meta:write(?ANN_TRAIN, #egTicTacAnnSample{skey=Key
                                            , qos=1, input=Input, output=Output}),
            {Hash, Output};
        [#egTicTacAnnSample{qos=N, input=Input, output=OldOut}] ->
            imem_meta:write(?ANN_TRAIN, #egTicTacAnnSample{skey=Key
                                            , qos=N+1, input=Input
                                            , output=vector_add(OldOut, Output)}),
            {Hash, Output};
        _ ->
            % hash collision detected, try to escape to negative hash
            AltKey = {GameTypeId, -Hash},
            case imem_meta:read(?ANN_TRAIN, AltKey) of
                [] ->   
                    imem_meta:write(?ANN_TRAIN, #egTicTacAnnSample{skey=AltKey, qos=1, input=Input, output=Output}),
                    {-Hash, Output};
                [#egTicTacAnnSample{qos=AN, input=Input, output=AltOut}] ->
                    imem_meta:write(?ANN_TRAIN, #egTicTacAnnSample{skey=AltKey
                                                , qos=AN+1, input=Input
                                                , output=vector_add(AltOut, Output)}),
                    {-Hash, Output};
                [#egTicTacAnnSample{input=CollInput}] ->
                    ?Info("Unrecoverable input hash collision OldInput= ~p  NewInput=~p ignored",[CollInput, Input]),
                    {0, Output}
            end
    end.

%% Output (gain) of a single sample output item depending on the MTE (moves to end)
%% An MTE of 0 (a win) is placed somewhat above the higest sensitivity of the AF
%% An MTE of 1 (a loose/tie) is placed somewhat below higest sensitivity in the negative
%% A (roughly) symmetric AF around the origin is assumed here
ann_sample_gain(BoardSize, Score, MTE) when Score >= 0.0 ->
    ?ANN_OUTPUT_TARGET * Score * (BoardSize-MTE) / (BoardSize-?ANN_OUTPUT_MTE);
ann_sample_gain(BoardSize, Score, MTE) ->
    ?ANN_OUTPUT_TARGET * Score * (BoardSize-MTE+1) / (BoardSize-?ANN_OUTPUT_MTE).

vector_add(OldOut, Output) ->
    [accu_add(Old, Out) || {Old, Out} <- lists:zip(OldOut, Output)].

accu_add(0, 0) -> 0;
accu_add({AccN,AccS}, {N,S}) -> {AccN+N, AccS+S}. 


ann_norm_input(L) -> [ann_norm_single_input(I) || I <- L].

ann_norm_single_input(32) -> 0.0;
ann_norm_single_input($X) -> 1.0;
ann_norm_single_input($O) -> -1.0;
ann_norm_single_input($*) -> 0.5;
ann_norm_single_input($$) -> -0.5;
ann_norm_single_input(I) -> I/256.0.

ann_norm_sample_output(AggregatedOutputVector, _QOS) ->
    Fun = fun(0) -> 0; ({0,_}) -> 0.001;  ({N,S}) -> S/N end, 
    lists:map(Fun, AggregatedOutputVector). 

% ann_norm_sample_output(AggregatedOutputVector, _QOS) ->
%     Fac = case norm(AggregatedOutputVector) of
%         0.0 ->      0.0;
%         0 ->        0.0;
%         Norm ->     1.0 / Norm
%     end,
%     Fun = fun(W) -> Fac * W end,  % Same vector length of 1 for all samples
%     lists:map(Fun, AggregatedOutputVector). 

% ann_norm_sample_output(AggregatedOutputVector, _QOS) ->
%   Fac = 1.0 / norm(AggregatedOutputVector),
%   Fun = fun(W) -> Fac * W end,  % Same vector length of 1 for all samples
%   lists:map(Fun, AggregatedOutputVector). 

% ann_norm_sample_output(AggregatedOutputVector, QOS) ->
%     Fun = fun(W) -> W/QOS end,  % Average over all Samples
%     lists:map(Fun, AggregatedOutputVector). 

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
                            {ok, Idx, NewBoard} = play_bot_ann(Board, Width, Height, Gravity, Periodic, IAliases, NAliases, Network, Flatten),
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
handle_info(Request, State) -> 
    ?Info("Unsolicited handle_info in ~p : ~p",[?MODULE, Request]),
    {noreply, State}.

handle_call(finish, _From, #state{network=Network} = State) ->
    Network ! {finish},
    {reply, ok, State#state{network=undefined}};
handle_call(start_learning, _From, State) ->
    {reply, ok, State#state{status=learning}};
handle_call({predict, NormBoard}, _From, #state{network=Network, status=playing} = State) ->
    NormIn = ann_norm_input(NormBoard),
    % ?Info("Network Input ~p ~p ",[Network, NormIn]),
    Out = ann:predict(Network, NormIn),
    % ?Info("Network Output ~p ~p ",[Network, Out]),
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


% -spec play_bot_immediate_win(binary(), integer(), integer(), integer(), boolean(), boolean(), egWinId(), [egAlias()], Options::[egGameMove()]) -> {ok, integer(), binary()} | {nok, no_immediate_win} | {error, atom()}.
% play_bot_immediate_win(_Board, _Width, _Height, _Run, _Gravity, _Periodic, _WinMod, _Aliases, []) -> {nok, no_immediate_win};  
% play_bot_immediate_win(Board, Width, Height, Run, Gravity, Periodic, WinMod, Aliases, [I|Rest]) -> 
%     {ok, Idx, TestBoard} = egambo_tictac:put(Gravity, Board, Width, I, hd(Aliases)),
%     case egambo_tictac:is_win(WinMod, TestBoard, Aliases) of
%         true ->     {ok, Idx, TestBoard};
%         false ->    play_bot_immediate_win(Board, Width, Height, Run, Gravity, Periodic, WinMod, Aliases, Rest)
%     end.

% -spec play_bot_defend_immediate(binary(), integer(), integer(), integer(), boolean(), boolean(), egWinId(), [egAlias()], Options::[egGameMove()]) -> egBotMove() | {nok, no_immediate_risk} | {error, atom()}.
% play_bot_defend_immediate(_Board, _Width, _Height, _Run, _Gravity, _Periodic, _WinMod, _Aliases, []) -> {nok, no_immediate_risk};
% play_bot_defend_immediate(Board, Width, Height, Run, Gravity, Periodic, WinMod, [Player|Others], [I|Rest]) -> 
%     {ok, Idx, TestBoard} = egambo_tictac:put(Gravity, Board, Width, I, hd(Others)),
%     case egambo_tictac:is_win(WinMod, TestBoard, Others) of
%         true ->     egambo_tictac:put(Gravity, Board, Width, Idx, Player);
%         false ->    play_bot_defend_immediate(Board, Width, Height, Run, Gravity, Periodic, WinMod, [Player|Others], Rest)
%     end.

-spec play_bot_ann(binary(), integer(), integer(), boolean(), boolean(), [egAlias()], [egAlias()], pid(), number()) -> egBotMove().
play_bot_ann(Board, Width, Height, Gravity, Periodic, IAliases, [Player|_]=NAliases, Network, Flatten) ->
    ?Info("play_bot_ann Board, NAliases ~p ~p", [Board, NAliases]),
    ABoard = egambo_tictac:norm_aliases(Board, NAliases, IAliases),
    ?Info("play_bot_ann ABoard after alias transform ~p ", [ABoard]),
    {NormBoard, Sym} = egambo_tictac_sym:norm(Width, Height, Gravity, Periodic, ABoard), 
    ?Info("play_bot_ann normalized Board and symmetry used ~p ~p", [NormBoard, Sym]),
    NormIn = ann_norm_input(binary_to_list(NormBoard)),
    ?Info("play_bot_ann Network, NormIn ~p ~p ", [Network, NormIn]),
    Output = ann:predict(Network, NormIn),
    ?Info("play_bot_ann network Output ~p",[Output]),    
    NormIdx = pick_output(Output, NormBoard, Flatten),      % 0-based index for predicted move
    NewIdx = egambo_tictac_sym:denorm_move(Width, Height, NormIdx, Sym),    
    ?Info("play_bot_ann NormIdx, NewIdx (output picked) ~p ~p", [NormIdx, NewIdx]),
    {ok, Idx, NewBoard} = egambo_tictac:put(Gravity, Board, Width, NewIdx, Player),
    ?Info("play_bot_ann Idx, NewBoard ~p ~p", [Idx, binary_to_list(NewBoard)]),
    {ok, Idx, NewBoard}.

-spec pick_output(list(), binary(), number()) -> integer().
pick_output(Output, NormBoard, 0) ->
    Valid = fun({_G, I}) -> (binary:part(NormBoard,I,1) == <<?AVAILABLE>>) end,
    {_MaxGain, ImaxGain} = hd(lists:reverse(lists:sort(lists:filter(Valid, lists:zip(Output, lists:seq(0, length(Output)-1)))))),
    ImaxGain;     % 0-based
pick_output(Output, NormBoard, Flatten) ->
    Valid = fun({_G, I}) -> (binary:part(NormBoard,I,1) == <<?AVAILABLE>>) end,
    SortedGainList = lists:sort(lists:filter(Valid, lists:zip(Output, lists:seq(0, length(Output)-1)))),
    {MaxGain, _} = lists:last(SortedGainList),
    Pred = if 
        MaxGain =< 0.0 ->   fun({G,_}) -> (G<MaxGain-1.0e-7) end; 
        true ->             fun({G,_}) -> (G<MaxGain*(1.0-Flatten)) end
    end,
    ShortList = lists:dropwhile(Pred, SortedGainList),
    element(2, lists:nth(rand:uniform(length(ShortList)), ShortList)).

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