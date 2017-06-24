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

-define(ANN_NO_TEST_SAMPLES, {error, <<"No test samples found">>}).
-define(ANN_BAD_OUTPUT, {error, <<"Bad Network Output">>}).
-define(ANN_NOT_LEARNING, {error, <<"Network is not in learning mode">>}).
-define(ANN_NOT_PLAYING, {error, <<"Network is not in playing mode">>}).

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
        , ann_sample/3
        , read_samples/6
        , predict/2
        ]).

% egambo_tictac_ann:resume(<<"tic_tac_toe_443">>).
% egambo_tictac_ann:start_learning(<<"tic_tac_toe_443">>).
% egambo_tictac_ann:train(<<"tic_tac_toe_443">>, 20    , 0.1   , 0.05, 0.49  , 1     , 150).
% egambo_tictac_ann:train(GameTypeId           , Epochs, ResErr, Rate, QosMin, QosMax, BatchSize) 
% egambo_tictac_ann:save(<<"tic_tac_toe_443">> , <<"V0.1">>,<<"First Attempt for Learning">>). 

-define(BOT_IS_BUSY, {error, bot_is_busy}).

-safe([state, start, stop, resume, save, start_learning, start_playing, ann_sample, train, test]).

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

train(GameTypeId, Epochs, ResErr, Rate, QosMin, QosMax, BatchSize) ->
    train(GameTypeId, Epochs, ResErr, Rate, QosMin, QosMax, BatchSize, -1.0e100, 1).

train(GameTypeId, Epochs, ResErr, Rate, QosMin, QosMax, BatchSize, LastKey, BatchId) -> 
    case read_samples(?ANN_TRAIN, GameTypeId, LastKey, BatchSize, QosMin, QosMax) of
        {[], true} -> 
            ok;
        {L, true} ->
            ?Info("Training ann for batch ~p of length ~p and game type ~s", [BatchId, length(L), GameTypeId]),
            TrainingSet = [ {ann_norm_input(I), O} || [_, _, I, O] <- L],
            % ?Info("First Sample ~p", [hd(TrainingSet)]),
            % ?Info("Last Sample ~p", [lists:last(TrainingSet)]),
            gen_server:call(?BOT_GID(?MODULE, GameTypeId), {train, Epochs, ResErr, Rate, TrainingSet}, infinity);
        {L, false} ->
            ?Info("Training ann for batch ~p of length ~p and game type ~s", [BatchId, length(L), GameTypeId]),
            TrainingSet = [ {ann_norm_input(I), O} || [_, _, I, O] <- L],
            % ?Info("First Sample ~p", [hd(TrainingSet)]),
            % ?Info("Last Sample ~p", [lists:last(TrainingSet)]),
            case gen_server:call(?BOT_GID(?MODULE, GameTypeId), {train, Epochs, ResErr, Rate, TrainingSet}, infinity) of
                Err when is_tuple(Err) ->  Err;
                ok -> train(GameTypeId, Epochs, ResErr, Rate, QosMin, QosMax, BatchSize, element(2, hd(lists:last(L))), BatchId+1)
            end
    end.

test(GameTypeId, QosMin, QosMax) ->
    case read_samples(?ANN_TEST, GameTypeId, -1.0e100, 1000000, QosMin, QosMax) of
        [[], true] -> 
            ?ANN_NO_TEST_SAMPLES;
        [L, _] ->
            TestSet = [ {I, O} || {_, _, I, O} <- L],
            gen_server:call(?BOT_GID(?MODULE, GameTypeId), {test, TestSet}, infinity)
    end.

predict(GameTypeId, NormBoard) ->
    gen_server:call(?BOT_GID(?MODULE, GameTypeId), {predict, NormBoard}).

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

read_samples(TableName, GameTypeId, LastKey, Limit, QosMin, QosMax) ->
    MatchCond = [{'>', '$1', match_val({GameTypeId, LastKey})}
                ,{'>', '$2', QosMin}
                ,{'=<', '$2', QosMax}
                ], 
    MatchFunction = {#egTicTacAnnSample{skey='$1', qos='$2', input='$3', output='$4'}, MatchCond, ['$$']},
    imem_meta:select(TableName, [MatchFunction], Limit).

match_val(T) when is_tuple(T) -> {const,T};
match_val(V) -> V.

ann_sample(GameTypeId, GameId, [Input, _Players, Move, [Score|_], MTE]) ->
    QOS = if 
        Score >= 0.0 ->  Score/(MTE div 2 +1);
        Score == 0.0 ->  0;
        true -> Score/((MTE+1) div 2)
    end,
    F = fun(I) -> if I==Move -> QOS; true -> 0 end end, 
    Output = lists:map(F, lists:seq(0, length(Input)-1)),
    #egTicTacAnnSample{skey={GameTypeId, GameId}, qos=QOS, input=Input, output=Output}.

ann_norm_input(L) -> [I/256.0 || I <- L].    

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
                {L, ann:create_neural_network(L),learning, <<>>, <<>>};
            [#egTicTacAnnModel{layers=L, weights=W, version=V, info=I}] ->
                {L, ann:create_neural_network(L, lists:flatten(W)), playing, V, I}
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
    [Width*Height|[Width*Height || _ <- lists:seq(2, Width)]].

start_link(GameTypeId)  ->
    gen_server:start_link(?BOT_GID(?MODULE, GameTypeId), ?MODULE, [GameTypeId], []).

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_cast({play_bot_req, GameId, _, [Player|_]}, #state{status=Status} = State) when Status /= playing ->
    play_bot_resp(GameId, Player, ?BOT_NOT_PLAYING),
    {noreply, State};
handle_cast({play_bot_req, GameId, Board, NAliases}, #state{ width=Width, height=Height, run=Run, network=Network
                                                          , gravity=Gravity, periodic=Periodic, winmod=WinMod
                                                          , ialiases=IAliases
                                                          } = State) -> 
    Options = put_options(Board, Width, Gravity),
    case play_bot_immediate_win(Board, Width, Height, Run, Gravity, Periodic, WinMod, NAliases, Options) of
        {ok, Idx, NewBoard} ->              % win in this move detected
            play_bot_resp(GameId, hd(NAliases), {ok, Idx, NewBoard}),
            {noreply, State}; 
        {nok, no_immediate_win} ->
            case play_bot_defend_immediate(Board, Width, Height, Run, Gravity, Periodic, WinMod, NAliases, Options) of
                {ok, Idx, NewBoard} ->      % opponent's win in this move detected and taken
                    play_bot_resp(GameId, hd(NAliases), {ok, Idx, NewBoard}),
                    {noreply, State};
                {nok, no_immediate_risk} ->
                    case play_bot_ann(Board, Width, Gravity, IAliases, [Player|_]=NAliases, Network) of 
                        {ok, Idx, NewBoard} ->
                            play_bot_resp(GameId, Player, {ok, Idx, NewBoard}),
                            {noreply, State}
                    end;
                Error -> 
                    play_bot_resp(GameId, hd(NAliases), Error),
                    {noreply, State}
            end;
        Error -> 
            play_bot_resp(GameId, hd(NAliases), Error),
            {noreply, State}
    end;
handle_cast(Request, State) -> 
    ?Info("Unsolicited handle_cast in ~p : ~p",[?MODULE, Request]),
    {noreply, State}.

handle_info(Request, State) -> 
    ?Info("Unsolicited handle_info in ~p : ~p",[?MODULE, Request]),
    {noreply, State}.

handle_call(finish, _From, #state{network=Network} = State) ->
    Network ! {finish},
    {reply, ok, State#state{network=undefined}};
handle_call(start_learning, _From, State) ->
    {reply, ok, State#state{status=learning}};
handle_call({predict, NormBoard}, _From, #state{network=Network, status=playing} = State) ->
    {reply, ann:predict(Network, ann_norm_input(NormBoard)), State};
handle_call({predict, _}, _From, State) ->
    {reply, ?ANN_NOT_PLAYING, State};
handle_call(start_playing, _From, State) ->
    {reply, ok, State#state{status=playing}};
handle_call({train, Epochs, ResErr, Rate, TrainingSet}, _From, #state{network=Network, status=learning} = State) ->
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
    imem_meta:write(?ANN_MODEL, #egTicTacAnnModel{tid=GameTypeId , version=Version, info=Info, layers=Layers, weights=ann:get_weights(Network)}),
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

put_options(Board, _Width, false) ->
    lists:usort([ case B of ?AVAILABLE -> I; _ -> false end || {B,I} <- lists:zip(binary_to_list(Board), lists:seq(0,size(Board)-1))]) -- [false];
put_options(Board, Width, true) ->
    lists:usort([ case B of ?AVAILABLE -> I; _ -> false end || {B,I} <- lists:zip(binary_to_list(binary:part(Board,0,Width)), lists:seq(0,Width-1))]) -- [false].

-spec play_bot_immediate_win(binary(), integer(), integer(), integer(), boolean(), boolean(), egWinId(), [egAlias()], Options::[egGameMove()]) -> {ok, integer(), binary()} | {nok, no_immediate_win} | {error, atom()}.
play_bot_immediate_win(_Board, _Width, _Height, _Run, _Gravity, _Periodic, _WinMod, _Aliases, []) -> {nok, no_immediate_win};  
play_bot_immediate_win(Board, Width, Height, Run, Gravity, Periodic, WinMod, Aliases, [I|Rest]) -> 
    {ok, Idx, TestBoard} = egambo_tictac:put(Gravity, Board, Width, I, hd(Aliases)),
    case WinMod:win(TestBoard, Aliases) of
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

-spec play_bot_ann(binary(), integer(), boolean(), [egAlias()], [egAlias()], pid()) -> egBotMove().
play_bot_ann(Board, Width, Gravity, IAliases, [Player|_]=NAliases, Network) ->
    NormBoard = egambo_tictac:norm_aliases(Board, NAliases, IAliases), 
    Output = ann:predict(Network, ann_norm_input([binary_to_list(NormBoard)])),
    pick_output(Output, Board, Width, Gravity, Player).


-spec pick_output(list(), binary(), integer(), boolean(), egAlias()) -> egBotMove().
pick_output(Output, Board, Width, Gravity, Player) ->
    SortedOutput = lists:sort([{Gain,I} || {Gain,I} <- lists:zip(Output, lists:seq(0, length(Output)-1))]),   
    pick_output_sorted(SortedOutput, Board, Width, Gravity, Player).

-spec pick_output_sorted(list(), binary(), integer(), boolean(), egAlias()) -> egBotMove().
pick_output_sorted([{_,I}|Outputs], Board, Width, Gravity, Player) ->
    case egambo_tictac:put(Gravity, Board, Width, I, Player) of
        {ok, Idx, NewBoard} ->  {ok, Idx, NewBoard};
        _ ->                    pick_output_sorted(Outputs, Board, Width, Gravity, Player)
    end.
