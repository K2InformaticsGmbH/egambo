-module(egambo_connect_four_bot).

-include("egambo_game.hrl").

-behavior(gen_server).

% Bot callbacks
-export([start_link/1
        ,resume/1]).

-export([process_games/0
        ,process_games/1
        ,new_game/1
        ,games/1
        ,remove_game/2
        ,move/3
        ,get_training_data/2
        ,suggest_moves/2
        ,save/1
        ,evaluate_position/2
        ,train/2
        ,train/4]).

% Gen server callbacks
-export([start/1
        ,start/2
        ,stop/1
        ,init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3]).

-define(TRAIN_CHANNEL, <<"connect_four_training">>).
%-define(TRAIN_CHANNEL, <<"c4_train_small">>).
-define(NETWORK_CHANNEL, <<"connect_four_network">>).
-define(LEARNING_RATE, 0.05).

%-define(NETWORK_NAME, "all_data_tf_500").
-define(NETWORK_NAME, "all_data_relu_220").
-define(NETWORK_LAYERS, [42,128,256,72,1]).
%-define(NETWORK_LAYERS, [42,128,512,108,1]).

-record(state, {
    id :: list(),
    network :: pid(), 
    games = #{} :: map()
}).

%% Functions required by egambo_game for bots.
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

start_link(GameTypeId)  ->
    InitArgs = [?NETWORK_NAME, ?NETWORK_LAYERS],
    gen_server:start_link(?BOT_GID(?MODULE, GameTypeId), ?MODULE, InitArgs, []).

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

%% End bot functions

-spec start(list()) -> {ok, pid()}.
start(Name) when is_list(Name) ->
    start(Name, ?NETWORK_LAYERS).

-spec start(list(), list()) -> {ok, pid()}.
start(Name, Layers) ->
    gen_server:start(?MODULE, [Name, Layers], []).

-spec stop(egBotId()) -> ok.
stop(GameTypeId) ->
    supervisor:terminate_child(egambo_bot_sup, ?BOT_ID(?MODULE, GameTypeId)),
    supervisor:delete_child(egambo_bot_sup, ?BOT_ID(?MODULE, GameTypeId)).

-spec new_game(pid()) -> integer().
new_game(BotId) ->
    gen_server:call(BotId, new_game).

-spec games(pid()) -> map().
games(BotId) ->
    gen_server:call(BotId, games).

-spec remove_game(pid(), integer()) -> ok.
remove_game(BotId, GameId) ->
    gen_server:call(BotId, {remove_game, GameId}).

-spec move(pid(), integer(), integer()) -> ok | {error, binary()}.
move(BotId, GameId, Pos) ->
    gen_server:call(BotId, {move, GameId, Pos}).

-spec suggest_moves(pid(), integer()) -> [{integer(), float()}].
suggest_moves(BotId, GameId) ->
    gen_server:call(BotId, {suggest_moves, GameId}).

-spec save(pid()) -> ok.
save(BotId) ->
    gen_server:call(BotId, save).

-spec evaluate_position(pid(), [integer()]) -> float().
evaluate_position(BotId, Pos) ->
    gen_server:call(BotId, {evaluate_position, Pos}).

-spec train(pid(), integer(), integer(), integer())  -> tuple() | end_of_data.
train(BotId, Batches, BatchSize, Epochs) ->
    gen_server:call(BotId, {train, Batches, BatchSize, Epochs, []}, infinity).

%% TODO: Should we keep the continuation info inside the bot state ?
-spec train(pid(), tuple() | end_of_data) -> tuple() | end_of_data.
train(_BotId, end_of_data) -> end_of_data;
train(BotId, {continue, Batches, BatchSize, Epochs, FromKey}) ->
    gen_server:call(BotId, {train, Batches, BatchSize, Epochs, FromKey}, infinity).

-spec process_games() -> ok.
process_games() ->
    Games = imem_meta:read(egGame),
    process_games(Games, 0).

-spec process_games(integer() | [#egGame{}]) -> ok.
process_games(Count) when is_integer(Count) ->
    Games = lists:sublist(imem_meta:read(egGame), Count),
    process_games(Games, 0).

-spec process_games(list(), integer()) -> ok.
process_games([], Count) ->
    ?Info("processing games finished, Processed ~p games", [Count]),
    ok;
process_games([#egGame{tid = <<"connect_four">>, status = finished, naliases = Next, nscores = RawScores, moves = Moves} | Rest], Count) ->
    %% TODO: we should get our id to know when we won only instead of all games...
    process_moves(lists:reverse(Moves), map_scores(Next, RawScores)),
    case Count rem 500 of
        0 -> ?Info("~p games processed", [Count]);
        _ -> ok
    end,
    process_games(Rest, Count +1);
process_games([_Ignored | Rest], Count) ->
    process_games(Rest, Count + 1).

-spec process_moves([{integer(), integer()}], map()) -> ok.
process_moves(Moves, Scores) ->
    process_moves(Moves, Scores, lists:duplicate(42, 0)).

-spec process_moves([{integer(), integer()}], map(), [integer()]) -> ok.
process_moves([], _, _) -> ok;
process_moves([{Player, Pos} | Moves], Scores, Board) ->
    %% TODO: We are using system, but probably shoud use our own user...
    NBoard = apply_move(Board, Pos),
    Score = maps:get(Player, Scores),
    NewValue = case imem_dal_skvh:read(system, ?TRAIN_CHANNEL, [NBoard]) of
        [] ->
            #{<<"n">> => 1, <<"value">> => [Score]};
        [#{cvalue := CVal}] ->
            #{<<"n">> := N, <<"value">> := [Value]} = imem_json:decode(CVal, [return_maps]),
            % Calculate new average.
            #{<<"n">> => N+1, <<"value">> => [(Value*N + Score)/(N+1)]}
    end,
    imem_dal_skvh:write(system, ?TRAIN_CHANNEL, NBoard, imem_json:encode(NewValue)),
    process_moves(Moves, Scores, NBoard).

-spec map_scores([integer()], [integer()]) -> map().
map_scores(Next, Scores) ->
    % 1 for win 0 for lose 0.5 for tie, normalize from -1,1...
    NormScores = [(S+1)/2 || S <- Scores],
    maps:from_list(lists:zip(Next, NormScores)).

-spec apply_move([integer()], integer()) -> [integer()] | {error, binary()}.
apply_move([], _) -> [];
apply_move([0 | Rest], 0) ->
    check_error(1, apply_move(Rest, -1));
apply_move([S | Rest], Pos) when Pos =/= 0 ->
    check_error(S * -1, apply_move(Rest, Pos-1));
apply_move(_Board, _Pos) -> {error, <<"Invalid move, cell already used.">>}.

-spec transform_board(binary(), integer()) -> [integer()].
transform_board(BoardBin, Alias) when is_binary(BoardBin)->
    transform_board(binary_to_list(BoardBin), Alias);
transform_board(Board, Alias) when is_list(Alias) ->
    transform_board(Board, hd(string:to_upper(Alias)));
transform_board([], _) -> [];
transform_board([32 | RestBoard], BotId) ->
    [0 | transform_board(RestBoard, BotId)];
transform_board([BotId | RestBoard], BotId) ->
    % -1 as the board is given from the opponent point of view.
    [-1 | transform_board(RestBoard, BotId)];
transform_board([_ | RestBoard], BotId) ->
    [1 | transform_board(RestBoard, BotId)].

-spec check_error(integer(), [integer()] | {error, binary()}) -> [integer()] | {error, binary()}.
check_error(_I, {error, _} = Error) -> Error;
check_error(I, L) -> [I | L].

-spec train(list(), pid(), integer(), integer(), integer(), [integer()], integer())  -> tuple() | end_of_data.
train(_Id, _NN, Batches, BatchSize, Epochs, FromKey, BatchCount) when BatchCount >= Batches ->
    {continue, Batches, BatchSize, Epochs, FromKey};
train(Id, NN, Batches, BatchSize, Epochs, FromKey, BatchCount) ->
    case get_training_data(FromKey, BatchSize) of
        [] ->
            ?Info("Training completed"),
            end_of_data;
        Data ->
            {NextKey, _} = lists:last(Data),
            ?Info("Batch ~p/~p Data processed so far ~p", [BatchCount+1, Batches, BatchCount*BatchSize]),
            %% TODO: This is async so we need to know when it is finished.
            %%       Also should we try to use more than one epoch per batch ? ...
            NN ! {learn_epochs, Epochs, ?LEARNING_RATE, Data},
            timer:sleep(Epochs * BatchSize * 5),
            save(Id, NN),
            train(Id, NN, Batches, BatchSize, Epochs, NextKey, BatchCount+1)
    end.

-spec get_training_data([integer()], integer()) -> [{[integer()], [integer()]}].
get_training_data(Start, Limit) ->
    decode_data(imem_dal_skvh:readGT(system, ?TRAIN_CHANNEL, Start, Limit)).

-spec decode_data([map()]) -> [{[integer()], [integer()]}].
decode_data([]) -> [];
decode_data([#{ckey := K, cvalue := BinVal} | Rest]) ->
    #{<<"value">> := Value} = imem_json:decode(BinVal, [return_maps]),
    [{K, Value} | decode_data(Rest)].

-spec empty_pos([integer()], [integer()], integer()) -> [integer()].
empty_pos(_Board, _, 7) -> [];
empty_pos(Board, [], Offset) ->
    empty_pos(Board, [35,28,21,14,7,0], Offset + 1);
empty_pos(Board, [Col | Rest], Offset) ->
    Pos = Col + Offset,
    % nth starts at 1 and Pos 0
    case lists:nth(Pos + 1, Board) of
        0 -> [Pos | empty_pos(Board, [35,28,21,14,7,0], Offset + 1)];
        _ -> empty_pos(Board, Rest, Offset)
    end.

-spec suggest_moves_impl([integer()], pid()) -> {list(), integer()}.
suggest_moves_impl(Board, NN) ->
    Moves = empty_pos(Board, [35,28,21,14,7,0], 0),
    NextBoards = [apply_move(Board, M) || M <- Moves],
    Values = ann:predict(NN, NextBoards),
    Suggested = lists:zip(Values, Moves),
    {_, Best} = lists:max(Suggested),
    {Suggested, Best}.

-spec load_weights(list()) -> list().
load_weights([_Name | Layers] = NetworkId) ->
    case imem_dal_skvh:read(system, ?NETWORK_CHANNEL, [NetworkId]) of
        [] ->
            N = ann:compute_neurons(Layers),
            %% We use our own initialization of the weights so we have some flexibility.
            [rand:uniform() * 0.5 - 0.25 || _ <- lists:seq(1, N)];
        [#{cvalue := CVal }] ->
            lists:flatten(imem_json:decode(CVal, [return_maps]))
    end.

-spec save(list(), pid()) -> ok.
save(Id, NN) ->
    Weights = ann:get_weights(NN),
    imem_dal_skvh:write(system, ?NETWORK_CHANNEL, Id, imem_json:encode(Weights)),
    ok.

-spec init_network([integer()], [float()]) -> pid() | {error, binary()}.
init_network([42 | Rest] = Layers, Weights) ->
    case lists:last(Rest) of
        1 -> ann:create_neural_network(Layers, Weights, relu);
        _ -> {error, <<"Output layer must contain only one neuron">>}
    end;
init_network(_, _) -> {error, <<"Input layer must contain 42 neuros">>}.

%% Gen Server callbacks.
init([Name, Layers]) ->
    imem_dal_skvh:create_check_channel(?TRAIN_CHANNEL, []),
    imem_dal_skvh:create_check_channel(?NETWORK_CHANNEL, []),
    Id = [Name | Layers],
    Weights = load_weights(Id),
    case init_network(Layers, Weights) of
        {error, Reason} -> {stop, Reason};
        NetworkPid -> {ok, #state{id = Id, network = NetworkPid}}
    end.

handle_call(new_game, _From, #state{games = Games} = State) ->
    GameId = rand:uniform(100000000000000),
    Board = lists:duplicate(42, 0),
    {reply, GameId, State#state{games = Games#{GameId => Board}}};
handle_call(games, _From, #state{games = Games} = State) ->
    {reply, Games, State};
handle_call({remove_game, GameId}, _From, #state{games = Games} = State) ->
    {reply, ok, State#state{games = maps:remove(GameId, Games)}};
handle_call({move, GameId, Pos}, _From, #state{games = Games} = State) ->
    case maps:get(GameId, Games, undefined) of
        undefined -> {reply, {error, <<"Game id not found">>}, State};
        Board ->
            case apply_move(Board, Pos) of
                {error, _} = Error -> {reply, Error, State};
                NewBoard ->
                    ?Info("The new board ~p", [NewBoard]),
                    {reply, ok, State#state{games = Games#{GameId => NewBoard}}}
            end
    end;
handle_call({suggest_moves, GameId}, _From, #state{games = Games, network = NN} = State) ->
    case maps:get(GameId, Games, undefined) of
        undefined -> {reply, {error, <<"Game id not found">>}, State};
        Board ->
            {reply, suggest_moves_impl(Board, NN), State}
    end;
handle_call({evaluate_position, Pos}, _From, #state{network = NN} = State) ->
    [[Result]] = ann:predict(NN, [Pos]),
    {reply, Result, State};
handle_call(save, _From, #state{id = Id, network = NN} = State) ->
    %% TODO: It should be automatic but it depends on the state of the network...
    save(Id, NN),
    {reply, ok, State};
handle_call({train, Batches, BatchSize, Epochs, FromKey}, _From, #state{id = Id, network = NN} = State) ->
    {reply, train(Id, NN, Batches, BatchSize, Epochs, FromKey, 0), State}.

%% This game id is not the same as our internal game ids, maybe we should change name...
handle_cast({play_bot_req, GameId, BoardBin, [Alias | Others]}, #state{network = NN} = State) ->
    Options = put_options(BoardBin),
    case play_bot_immediate_win(BoardBin, Alias, Options) of
        {ok, Idx, NewBoard} ->
            play_bot_resp(GameId, Alias, {ok, Idx, NewBoard});
        {nok, no_immediate_win} ->
            case play_bot_defend_immediate(BoardBin, Alias, hd(Others), Options) of
                {ok, Idx, NewBoard} ->
                    play_bot_resp(GameId, Alias, {ok, Idx, NewBoard});
                {nok, no_immediate_risk} ->
                    Board = transform_board(BoardBin, Alias),
                    {_Sugested, Best} = suggest_moves_impl(Board, NN),
                    %% For some reason we need to give back the new board ...
                    play_bot_resp(GameId, Alias, egambo_tictac:put(true, BoardBin, 7, Best, Alias))
            end
    end,
    {noreply, State};
handle_cast(Request, State) -> 
    ?Info("Unsolicited handle_cast in ~p : ~p", [?MODULE, Request]),
    {noreply, State}.
  
handle_info(Request, State) -> 
    ?Info("Unsolicited handle_info in ~p : ~p", [?MODULE, Request]),
    {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%%% Functions copied/adapted from tic-tac-bot
put_options(Board) ->
    lists:usort([ case B of 32 -> I; _ -> false end || {B,I} <- lists:zip(binary_to_list(binary:part(Board, 0, 7)), lists:seq(0, 6))]) -- [false].

-spec play_bot_immediate_win(binary(), egAlias(), Options::[egGameMove()]) -> {ok, integer(), binary()} | {nok, no_immediate_win} | {error, atom()}.
play_bot_immediate_win(_Board, _Alias, []) -> {nok, no_immediate_win}; 
play_bot_immediate_win(Board, Alias, [I|Rest]) -> 
    {ok, Idx, TestBoard} = egambo_tictac:put(true, Board, 7, I, Alias),
    case egambo_tictac_win_7_6_4:win(TestBoard, Alias) of
        true ->     {ok, Idx, TestBoard};
        false ->    play_bot_immediate_win(Board, Alias, Rest)
    end.

-spec play_bot_defend_immediate(binary(), egAlias(), egAlias(), Options::[egGameMove()]) -> egBotMove() | {nok, no_immediate_risk} | {error, atom()}.
play_bot_defend_immediate(_Board, _Alias, _Enemy, []) -> {nok, no_immediate_risk};
play_bot_defend_immediate(Board, Alias, Enemy, [I|Rest]) -> 
    {ok, Idx, TestBoard} = egambo_tictac:put(true, Board, 7, I, Enemy),
    case egambo_tictac_win_7_6_4:win(TestBoard, Enemy) of
        true ->     egambo_tictac:put(true, Board, 7, Idx, Alias);
        false ->    play_bot_defend_immediate(Board, Alias, Enemy, Rest)
    end.
