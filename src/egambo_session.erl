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

-record(state, {
    playerId :: egAccountId(),
    skey :: integer(),
    sessionId :: binary()
}).

-spec start_link(binary()) -> {ok, pid()} | {error, term()}.
start_link(SessionId) ->
    gen_server:start_link({global, SessionId}, ?MODULE, [SessionId], []).

-spec request(binary(), map(), fun()) -> ok | {error, binary()}.
request(SessionId, #{<<"action">> := Action} = ReqArgs, ReplyFun) ->
    case extract_args(Action, ReqArgs) of
        invalid -> ReplyFun(<<"Invalid arguments or action not implemented">>);
        Args -> gen_server:cast({global, SessionId}, {Action, Args, ReplyFun})
    end;
request(_SessionId, _ReqArgs, ReplyFun) when is_function(ReplyFun) ->
    ReplyFun(<<"Invalid request">>).

init([SessionId]) ->
    {ok, #state{sessionId = SessionId}}.

handle_call(_Req, _From, State) ->
    {reply, {error, <<"invalid request">>}, State}.

handle_cast({<<"login">>, {User, Password}, ReplyFun}, #state{playerId = undefined, sessionId = SessionId} = State) ->
    case authenticate(User, Password, SessionId) of
        {error, invalid} ->
            ReplyFun(<<"Invalid account credentials">>),
            {noreply, State};
        {error, expired} ->
            ReplyFun(#{expired => true, msg => list_to_binary(?PasswordChangeNeeded)}),
            {noreply, State};
        {error, internal_error} ->
            ReplyFun(<<"Server error validating credentials">>),
            {noreply, State};
        SKey ->
            {ok, PlayerId} = get_userid(SKey, User),
            ReplyFun(<<"ok">>),
            {noreply, State#state{playerId = PlayerId, skey = SKey}}
    end;
handle_cast({<<"change_credentials">>, {User, Password, NewPassword}, ReplyFun}, #state{sessionId = SessionId} = State) ->
    % TODO: Do we really need a configurable policy, also we might want
    %       to have a stronger policy for dderl users, suggested
    %       at least 12 characters containing a letter:
    %       <<"\nfun (Password) ->\n\tREStrong =\n\t    \"^(?=.{12,})(((?=.*[a-z]))|((?=.*[A-Z]))).*$\",\n\tREEnough = \"(?=.{12,}).*\",\n\tcase re:run(Password, REEnough) of\n\t  nomatch -> short;\n\t  _ ->\n\t      case re:run(Password, REStrong) of\n\t\tnomatch -> weak;\n\t\t_ -> strong\n\t      end\n\tend\nend\n">>

    StrengthFun = imem_seco:password_strength_fun(),
    case StrengthFun(NewPassword) of
        strong ->
            case change_credentials(User, Password, NewPassword, SessionId) of
                {error, invalid} ->
                    ReplyFun(<<"Invalid account credentials">>),
                    {noreply, State};
                {error, internal_error} ->
                    ReplyFun(<<"Server error validating credentials">>),
                    {noreply, State};
                SKey ->
                    {ok, PlayerId} = get_userid(SKey, User),
                    ReplyFun(<<"ok">>),
                    {noreply, State#state{playerId = PlayerId, skey = SKey}}
            end;
        short ->
            ReplyFun(<<"Your password must be at least 12 characters long">>),
            {noreply, State};
        weak ->
            ReplyFun(<<"Your password must contain at least one letter">>),
            {noreply, State};
        _ ->
            ReplyFun(<<"Error checking password strength">>),
            {noreply, State}
    end;
handle_cast({_Action, _Args, ReplyFun}, #state{playerId = undefined} = State) ->
    ReplyFun(<<"User not logged in">>),
    {noreply, State};
handle_cast({<<"new_game">>, {GameType, Opponent}, ReplyFun}, #state{skey = SKey} = State) when is_binary(Opponent) ->
    case get_userid(SKey, Opponent) of
        {ok, OpponentId} ->
            handle_cast({<<"new_game">>, {GameType, OpponentId}, ReplyFun}, State);
        {error, not_found} ->
            ReplyFun(<<"Opponent account not found">>),
            {noreply, State};
        {error, Error} ->
            ReplyFun(iolist_to_binary(io_lib:format("~p", [Error]))),
            {noreply, State}
    end;
handle_cast({<<"new_game">>, {GameType, OpponentId}, ReplyFun}, #state{playerId = PlayerId} = State) ->
    Result = egambo_game:start(GameType, OpponentId, PlayerId),
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
extract_args(<<"login">>, #{<<"user">> := User, <<"password">> := Password}) when
            is_binary(User), is_binary(Password) -> {User, Password};
extract_args(<<"change_credentials">>, #{<<"user">> := User, <<"password">> := Password,
            <<"new_password">> := NewPassword}) when is_binary(User),
            is_binary(Password), is_binary(NewPassword) -> {User, Password, NewPassword};
extract_args(<<"new_game">>, #{<<"type">> := GameType, <<"opponent">> := Op}) when
            is_binary(GameType), is_integer(Op) orelse is_binary(Op) -> {GameType, Op};
extract_args(<<"new_game">>, #{<<"type">> := GameType}) when is_binary(GameType) -> {GameType};
extract_args(<<"accept">>, #{<<"game_id">> := GameId}) when is_integer(GameId) -> {GameId};
extract_args(<<"cancel">>, #{<<"game_id">> := GameId}) when is_integer(GameId) -> {GameId};
extract_args(<<"play">>, #{<<"game_id">> := GameId, <<"position">> := Pos}) when
            is_integer(GameId), is_integer(Pos) -> {GameId, Pos};
extract_args(_Action, _ReqArgs) -> invalid.

-spec unwrap_error_new_game(fun(), {error, term()} | integer()) -> ok.
unwrap_error_new_game(ReplyFun, {error, Error}) ->
    ReplyFun(iolist_to_binary(io_lib:format("Error creating the game: ~p", [Error])));
unwrap_error_new_game(ReplyFun, GameId) ->
    ReplyFun(#{msg => <<"Game created">>, id => GameId}).

-spec authenticate(binary(), binary(), binary()) -> integer() | {error, invalid | internal | expired}.
authenticate(User, Password, SessionId) ->
    Md5Pass = erlang:md5(Password),
    try
        SKey = imem_sec:authenticate(undefined, SessionId, User, {pwdmd5, Md5Pass}),
        imem_sec:login(SKey)
    catch
        throw:{'SecurityException', {?PasswordChangeNeeded, _AccountId}} ->
            ?Info("Password expired for user ~p", [User]),
            {error, expired};
        throw:{'SecurityException', Msg} ->
            ?Info("User ~p login failed, msg: ~p", [User, Msg]),
            {error, invalid};
        Error:Reason ->
            ?Error("The error and the reason ~p", [{Error, Reason}]),
            {error, internal_error}
    end.

-spec change_credentials(binary(), binary(), binary(), binary()) -> integer() | {error, invalid | internal}.
change_credentials(User, Password, NewPassword, SessionId) ->
    Md5Pass = erlang:md5(Password),
    try
        SKey = imem_sec:authenticate(undefined, SessionId, User, {pwdmd5, Md5Pass}),
        Md5NewPass = erlang:md5(NewPassword),
        imem_sec:change_credentials(SKey, {pwdmd5, Md5Pass}, {pwdmd5, Md5NewPass})
    catch
        throw:{'SecurityException', Msg} ->
            ?Info("User ~p login failed, msg: ~p", [User, Msg]),
            {error, invalid};
        Error:Reason ->
            ?Error("The error and the reason ~p", [{Error, Reason}]),
            {error, internal_error}
    end.

-spec get_userid(integer(), binary()) -> {ok, ddEntityId()} | {error, unauthorized}.
get_userid(SKey, User) when is_integer(SKey), is_binary(User) ->
    try
        UserId = imem_account:get_id_by_name(SKey,User),
        {ok, UserId}
    catch
        throw:{'ClientError', {"Account does not exist", User}} -> {error, not_found};
        _:Error -> {error, Error}
    end.



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