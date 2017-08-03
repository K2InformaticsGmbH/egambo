-module(egambo_session).
-behaviour(gen_server).

-include("egambo.hrl").

-export([
    start_link/1,
    request/3
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {playerId}).

-spec start_link(binary()) -> {ok, pid()} | {error, term()}.
start_link(SessionId) ->
    gen_server:start_link({global, SessionId}, ?MODULE, [], []).

-spec request(binary(), map(), fun()) -> ok | {error, binary()}.
request(SessionId, #{<<"action">> := Action} = ReqArgs, ReplyFun) ->
    case extract_args(Action, ReqArgs) of
        invalid -> {error, <<"Invalid arguments or action not implemented">>};
        Args -> gen_server:cast({global, SessionId}, {Action, Args, ReplyFun})
    end.

init([]) ->
    {ok, #state{}}.

handle_call(_Req, _From, State) ->
    {reply, {error, <<"invalid request">>}, State}.

handle_cast({<<"login">>, {User, _Password}, ReplyFun}, #state{playerId = undefined} = State) ->
    %% TODO: Implement proper auth, for now just get a valid userid.
    Account = #ddAccount{id='$1',name=User,_='_'},
    case imem_meta:select(ddAccount, [{Account, [], ['$1']}], 1) of
        {[], _} ->
            ReplyFun(<<"Invalid account credentials">>),
            {noreply, State};
        {[PlayerId], _} ->
            ReplyFun(<<"Login success">>),
            {noreply, #state{playerId = PlayerId}}
    end;
handle_cast({_Action, _Args, ReplyFun}, #state{playerId = undefined} = State) ->
    ReplyFun(<<"User not logged in">>),
    {noreply, State};
handle_cast({<<"new_game">>, {GameType, Opponent}, ReplyFun}, #state{playerId = PlayerId} = State) ->
    Result = egambo_game:start(GameType, Opponent, PlayerId),
    unwrap_error_new_game(ReplyFun, Result),
    {noreply, State};
handle_cast({<<"new_game">>, {GameType}, ReplyFun}, #state{playerId = PlayerId} = State) ->
    Result = egambo_game:start(GameType, PlayerId),
    unwrap_error_new_game(ReplyFun, Result),
    {noreply, State};
handle_cast({<<"accept">>, {GameId}, ReplyFun}, #state{playerId = PlayerId} = State) ->
    Result = egambo_game:accept(GameId, PlayerId),
    ReplyFun(iolist_to_binary(io_lib:format("~p", [Result]))),
    {noreply, State};
handle_cast({<<"cancel">>, {GameId}, ReplyFun}, #state{playerId = PlayerId} = State) ->
    Result = egambo_game:cancel(GameId, PlayerId),
    ReplyFun(iolist_to_binary(io_lib:format("~p", [Result]))),
    {noreply, State};
handle_cast({<<"play">>, {GameId, Pos}, ReplyFun}, #state{playerId = PlayerId} = State) ->
    Result = egambo_game:play(GameId, Pos, PlayerId),
    ReplyFun(iolist_to_binary(io_lib:format("~p", [Result]))),
    {noreply, State};
handle_cast({_, _, ReplyFun}, State) when is_function(ReplyFun) ->
    ReplyFun(<<"Invalid request">>),
    {noreply, State};
handle_cast(Request, State) ->
    ?Info("Unsolicited handle_cast in ~p : ~p", [?MODULE, Request]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec extract_args(binary(), map()) -> tuple().
extract_args(<<"login">>, #{<<"user">> := User, <<"password">> := Password}) -> {User, Password};
extract_args(<<"new_game">>, #{<<"type">> := GameType, <<"opponent">> := Op}) -> {GameType, Op};
extract_args(<<"new_game">>, #{<<"type">> := GameType}) -> {GameType};
extract_args(<<"accept">>, #{<<"game_id">> := GameId}) -> {GameId};
extract_args(<<"cancel">>, #{<<"game_id">> := GameId}) -> {GameId};
extract_args(<<"play">>, #{<<"game_id">> := GameId, <<"position">> := Pos}) -> {GameId, Pos}.

-spec unwrap_error_new_game(fun(), {error, term()} | integer()) -> ok.
unwrap_error_new_game(ReplyFun, {error, Error}) ->
    ReplyFun(iolist_to_binary(io_lib:format("Error creating the game: ~p", [Error])));
unwrap_error_new_game(ReplyFun, GameId) ->
    ReplyFun(#{msg => <<"Game created">>, id => GameId}).

% egambo_game:create(<<"tic_tac_toe">>, 2).
% egambo_game:create(<<"tic_tac_toe">>, 1, 2).
% egambo_game:start(<<"tic_tac_toe">>,  2).
% egambo_game:start(<<"tic_tac_toe">>, 1,  2).
% egambo_game:cancel(926946506377236097, 2).
% egambo_game:accept(1227778950635753473, 2).
% egambo_game:accept(1227778950635753473, 1).
% egambo_game:status(GameId, 2).
% egambo_game:play(664820677776998785, 7, $X, 2).
% egambo_game:result(664820677776998785).
% egambo_game:moves(664820677776998785).
% egambo_game:play(72673005093445425,a1).
% egambo_game:create(<<"tic_tac_toe">>, 10, 1,4).