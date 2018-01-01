-module(egambo_tictac_bot_ql).

-include("egambo_tictac.hrl").  % import tictac game definitions 

-behavior(gen_server).          % callbacks provided by (bot) players
%-behavior(egambo_gen_player).   % callbacks provided by (bot) players

-define(QL_TRAIN, "egTicTacQl_").           % Q-table name prefix (egGameTypeId to be appended)
-define(QL_TRAIN_OPTS, [{record_name, egTicTacQlSample}, {type, set}]).        
-define(QL_OUTPUT_UNSAMPLED, 0.001).        % Value emitted as output for unsampled move in Export
-define(QL_ALFA, 0.9). 
-define(QL_GAMMA, -0.9).    % Next move (action) is from opponent. Reward Q(s,a) needs to be negated 

-record(egTicTacQlSample,   { sin= <<>> :: binary() % binstr key of normalized position (Board)
                            , nos=1     :: number() % number of samples (1...)
                            , qos=1     :: number() % score of sample (-1..+1)
                            , aaq=[]    :: list()   % aggregated action qualities
                            }).

-define(egTicTacQlSample,   [ binstr
                            , number
                            , number
                            , list
                            ]).
% rd(egTicQlSample, {sin=, nos=, qos=, aaq=}).

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
                , explore=1.0 :: float()        % rate for exploration (0..1.0)
                , flatten=0.01 :: float()       % 
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
        ]).

-safe([state, stop, resume, explore, flatten]).

game_types(egambo_tictac) -> all;
game_types(_) -> [].

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
    {NormBoard, Sym} = egambo_tictac_sym:norm(Width, Height, Gravity, Periodic, ABoard), 
    % ?Info("play_bot_impl normalized Board and symmetry used ~p ~p", [NormBoard, Sym]),
    {NOS, AccActionQuality} = case imem_meta:read(Table, list_to_binary(NormBoard)) of
        [] ->   {0, new_output(NormBoard, Width, Height, Gravity)};
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
target({N,Q}, NOS, Explore) -> Q/N + Explore*math:sqrt(NOS)/(1+N).

