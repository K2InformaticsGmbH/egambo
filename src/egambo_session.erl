-module(egambo_session).
-behaviour(gen_server).

-include("egambo.hrl").
-include("egambo_game.hrl").

-export([
    start_link/3,
    get_xsrfToken/1,
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
    sessionId :: binary(),
    xsrf :: binary(),
    notifyFun :: fun()
}).

-spec start_link(binary(), binary(), fun()) -> {ok, pid()} | {error, term()}.
start_link(SessionId, XSRFToken, NotifyFun) ->
    gen_server:start_link({global, SessionId}, ?MODULE, [SessionId, XSRFToken, NotifyFun], []).

-spec get_xsrfToken(binary()) -> binary().
get_xsrfToken(SessionId) ->
    gen_server:call({global, SessionId}, get_xsrfToken).

-spec request(binary(), map(), fun()) -> ok | {error, binary()}.
request(SessionId, #{<<"action">> := Action} = ReqArgs, ReplyFun) ->
    case extract_args(Action, ReqArgs) of
        invalid -> ReplyFun(<<"Invalid arguments or action not implemented">>);
        Args ->
            gen_server:cast({global, SessionId}, {Action, Args, ReplyFun})
    end;
request(SessionId, ReqArgs, ReplyFun) when is_function(ReplyFun) ->
    ?Error("Invalid request for ~p with args: ~p", [SessionId, ReqArgs]),
    ReplyFun(<<"Invalid request">>).

init([SessionId, XSRFToken, NotifyFun]) ->
    {ok, #state{sessionId = SessionId, xsrf = XSRFToken, notifyFun = NotifyFun}}.

handle_call(get_xsrfToken, _From, State) ->
    ?Debug("get_xsrfToken, result: ~p~n", [State#state.xsrf]),
    {reply, State#state.xsrf, State};
handle_call(_Req, _From, State) ->
    {reply, {error, <<"invalid request">>}, State}.

handle_cast({<<"get_game_types">>, {}, ReplyFun}, #state{playerId = PlayerId} = State) ->
    ReplyFun(get_game_types(PlayerId)),
    {noreply, State};
handle_cast({<<"login">>, {User, Password}, ReplyFun}, #state{playerId = undefined, sessionId = SessionId, notifyFun = NotifyFun} = State) ->
    %% TODO: Delete secos as we don't need them...
    ?Info("Login attempt..."),
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
            case egambo_player:register(PlayerId, NotifyFun) of
                ok ->
                    ReplyFun(<<"ok">>),
                    {noreply, State#state{playerId = PlayerId, skey = SKey}};
                {error, Reason} ->
                    %% TODO: Review seco as probably this will leak :)
                    ReplyFun(Reason),
                    {noreply, State}
            end
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
handle_cast({<<"logout">>, _Args, ReplyFun}, #state{playerId = PlayerId} = State) ->
    ?Info("Terminating session for player ~p", [PlayerId]),
    ReplyFun(<<"ok">>),
    {stop, normal, State};
handle_cast({_Action, _Args, ReplyFun}, #state{playerId = undefined} = State) ->
    ReplyFun(<<"User not logged in">>),
    {noreply, State};
handle_cast({<<"list_games">>, {Type}, ReplyFun}, #state{playerId = PlayerId} = State) ->
    ?Info("Asking for the list of games of ~p", [{Type, PlayerId}]),
    ReplyFun(list_games(PlayerId, Type)),
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
handle_cast({<<"load_game">>, {GameId}, ReplyFun}, #state{} = State) ->
    case egambo_game:read_game(GameId) of
        {error, Error} -> ReplyFun(Error);
        #egGame{gid=GId, tid=Type, status=Status, board=Board, moves=MovesAlias} = Game ->
            % Ids need to be sent as string otherwise javascript
            % precision problems might arise.
            Id = integer_to_binary(GId),
            Players = [get_username(PlayerId) || PlayerId <- Game#egGame.nmovers],
            % As 88 is always first we just take list of moves and reverse the order
            % to make first move, the first element on the list.
            Moves = lists:reverse([Pos || {_ , Pos} <- MovesAlias]),
            %% We need to read the type to know the width of the board...
            #egGameType{params = #{ width := Width }} = egambo_game:read_type(Type),
            Height = size(Board) div Width,
            ReplyFun(#{
                id => Id,
                players => Players,
                status => Status,
                board => Board,
                moves => Moves,
                width => Width,
                height => Height
            })
    end,
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
handle_cast({Action, Parameters, ReplyFun}, State) when is_function(ReplyFun) ->
    ?Info("Unhandled action ~p received with parameters: ~p", [Action, Parameters]),
    ReplyFun(<<"Invalid request">>),
    {noreply, State};
handle_cast(Request, #state{sessionId = SessionId, playerId = PlayerId} = State) ->
    ?Error("Unsolicited cast ~p session ~p for player ~p", [Request, SessionId, PlayerId]),
    {noreply, State}.

handle_info(Info, #state{sessionId = SessionId, playerId = PlayerId} = State) ->
    ?Error("Unsolicited message ~p session ~p for player ~p", [Info, SessionId, PlayerId]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec extract_args(binary(), map()) -> tuple().
extract_args(<<"get_game_types">>, _Args) -> {};
extract_args(<<"list_games">>, #{<<"type">> := Type}) -> {Type};
extract_args(<<"login">>, #{<<"user">> := User, <<"password">> := Password}) when
            is_binary(User), is_binary(Password) -> {User, Password};
extract_args(<<"logout">>, _Args) -> {};
extract_args(<<"change_credentials">>, #{<<"user">> := User, <<"password">> := Password,
            <<"new_password">> := NewPassword}) when is_binary(User),
            is_binary(Password), is_binary(NewPassword) -> {User, Password, NewPassword};
extract_args(<<"new_game">>, #{<<"type">> := GameType, <<"opponent">> := Op}) when
            is_binary(GameType), is_integer(Op) orelse is_binary(Op) -> {GameType, Op};
extract_args(<<"new_game">>, #{<<"type">> := GameType}) when is_binary(GameType) -> {GameType};
extract_args(<<"accept">>, #{<<"game_id">> := GameId}) when is_integer(GameId) -> {GameId};
extract_args(<<"cancel">>, #{<<"game_id">> := GameId}) when is_integer(GameId) -> {GameId};
extract_args(<<"play">>, #{<<"game_id">> := GameId} = Args) when is_binary(GameId) -> 
    extract_args(<<"play">>, Args#{<<"game_id">> := binary_to_integer(GameId)});
extract_args(<<"play">>, #{<<"game_id">> := GameId, <<"position">> := Pos}) when
            is_integer(GameId), is_integer(Pos) -> {GameId, Pos};
extract_args(<<"load_game">>, #{<<"id">> := GameId}) when
            is_binary(GameId) -> {binary_to_integer(GameId)};
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

-spec get_userid(integer(), binary()) -> {ok, ddEntityId()} | {error, not_found}.
get_userid(SKey, User) when is_integer(SKey), is_binary(User) ->
    case imem_meta:select(ddAccount, [{#ddAccount{name = User, _='_'}, [], ['$_']}]) of
        {[#ddAccount{id = Id}], _} -> {ok, Id};
        _ -> {error, not_found}
    end.

-spec get_game_types(ddEntityId()) -> list().
get_game_types(undefined) ->
    %% TODO: This needs revisit, should the egambo_game module be the one
    %%       knowing the records ? maybe egambo_dal ?...
    Types = imem_meta:read(egGameType),
    [#{id => Id, name => Name, difficulty => Level, description => Info} ||
        #egGameType{tid = Id, tname = Name, level = Level, info = Info} <- Types];
get_game_types(PlayerId) ->
    ?Info("Missing count of games for authenticatd users, playerid: ~p", [PlayerId]),
    get_game_types(undefined).

-spec list_games(ddEntityId(), binary()) -> list().
list_games(PlayerId, Type) ->
    {Games, _} = imem_meta:select(egGame, [
        {#egGame{players = [PlayerId, '_'], tid = Type, _ = '_'}, [], ['$_']},
        {#egGame{players = ['_', PlayerId], tid = Type, _ = '_'}, [], ['$_']}
    ]),
    % Ids need to be sent as string otherwise javascript precision problems might arise.
    [#{id => integer_to_binary(Id), nmoves => length(Moves)} ||
        #egGame{gid = Id, moves = Moves} <- Games].

-spec get_username(ddEntityId) -> binary() | {error, not_found}.
get_username(UserId) ->
    case imem_meta:read(ddAccount, UserId) of
        [#ddAccount{name = Username} | _] -> Username;
        _ -> {error, not_found}
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