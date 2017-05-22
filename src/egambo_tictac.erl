-module(egambo_tictac).

% -include("egambo.hrl").         % import logging macros
-include("egambo_game.hrl").    % import game managing structures 

-behavior(gen_server).          % this is implemented on a gen_server basis
-behavior(egambo_gen_game).     % callbacks used by player (mediated through egambo_game)
-behavior(egambo_gen_engine).   % callbacks used by egambo_game (game manager)

-define(COLS, "abcdefgh").
-define(ROWS, "12345678").
-define(OBSTACLE, $$).
-define(JOKER, $*).
-define(AVAILABLE, 32).         % spac
-define(AUTOSAVE_PERIOD, 3000). % msec between state save to db

-define(NOT_YOUR_TURN, {error, not_your_turn}).
-define(NOT_PLAYING, {error, not_playing}).
-define(INVALID_CELL, {error, invalid_cell}).
-define(ALREADY_OCCUPIED, {error, already_occupied}).
-define(INVALID_ALIAS_PARAMETER, {error, invalid_alias_parameter}).
-define(INVALID_WIDTH_PARAMETER, {error, invalid_width_parameter}).
-define(INVALID_HEIGHT_PARAMETER, {error, invalid_height_parameter}).
-define(INVALID_RUN_PARAMETER, {error, invalid_run_parameter}).
-define(INVALID_BOARD_PARAMETER, {error, invalid_board_parameter}).
-define(INVALID_PARAMETER_CONFIG, {error, invalid_parameter_config}).

-include("egambo_game.hrl").

-type egTicTacParams() ::  #{ width => integer()
                            , height => integer()
                            , run => integer()
                            , gravity => boolean()
                            , periodic => boolean()
                            , obstacles => [integer()] | integer()
                            , jokers => [integer()] | integer()
                            , aliases => [integer()]
                            }.

-record(state,  { gid        :: egGameId()  % game id
                , tid        :: egGameTypeId()
                , width      :: integer()   % board width >= 3
                , height     :: integer()   % board height >= 3
                , run        :: integer()   % sucess run length
                , gravity    :: boolean()   % do moves fall towards higher row numbers
                , periodic   :: boolean()   % unbounded repeating board 
                , players  = []             :: [egAccountId()]     % hd(players) is owner (proposer) of the game 
                , bots     = []             :: [module()]          % internal bot player modules (undefined for external players)
                , etime    = undefined      :: egTime()            % last status change time (so far), game end time (eventually)
                , space    = <<>>           :: binary()            % initial board state
                , status   = forming        :: egGameStatus()      % current game (management) status
                , board    = <<>>           :: binary()            % current board (game state) before next move
                , nmovers  = []             :: [egAccountId()]     % next player AccountId enumeration for coming moves
                , naliases = []             :: [egAlias()]         % next player aliases (codes) for next moves
                , nscores  = []             :: [egScore()]         % player scores in next mover order (not player order)
                , moves    = []             :: [{egAlias(),integer()}] % reversed list of moves by mover0, mover1, mover0, ...                 }
                }).
% state,  { width=, height=, run=, gravity=, periodic=, players=, bots=, etime=, status=, board=, nmovers=, naliases=, nscores=, moves=}

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
-export([ prepare/2     % initialize the game vector on behalf of the game manager (stateless)
        , resume/1      % instruct supervisor to start the game engine process and resume the game
        , stop/1        % instruct supervisor to stop the game engine process (no attempt to save the state)
        ]).

% egambo_gen_game behavior callbacks (player originating requests)
-export([ play/2        % play one move in the name of the next player:         GameId, Cell
        , play/3        % play one move for the given alias:                    GameId, Cell, Alias
        , play/4        % play one move for given alis and AccountId:           GameId, Cell|Command, Alias, AccountId
        , result/1      % return current game state as a json-ready map:        GameId|#egGame{}
        , moves/1       % return game history as a json-ready map               GameId|#egGame{}
        ]).

% debugging API
-export([ state/1
        , print/1
        ]).

-spec validate_params(egTicTacParams()) -> ok | egGameError().
validate_params(#{width:=Width, height:=Height, run:=Run, gravity:=Gravity, periodic:=Periodic, obstacles:=Obstacles, jokers:=Jokers, aliases:=Aliases}) -> 
    MaxDim = max(Width, Height),
    if
        is_list(Aliases) == false ->                                ?INVALID_ALIAS_PARAMETER;
        length(Aliases) /= 2 ->                                     ?INVALID_ALIAS_PARAMETER;
        is_integer(hd(Aliases)) == false ->                         ?INVALID_ALIAS_PARAMETER;
        is_integer(hd(tl(Aliases))) == false ->                     ?INVALID_ALIAS_PARAMETER;
        is_integer(Width)==false ->                                 ?INVALID_WIDTH_PARAMETER;
        (Width<3) or (Width>8) ->                                   ?INVALID_WIDTH_PARAMETER;
        is_integer(Height)==false ->                                ?INVALID_HEIGHT_PARAMETER;
        (Height<3) or (Height>8) ->                                 ?INVALID_HEIGHT_PARAMETER;
        is_integer(Run)==false ->                                   ?INVALID_RUN_PARAMETER;
        (Run<3) or (Run>MaxDim) ->                                  ?INVALID_RUN_PARAMETER;
        is_boolean(Gravity)==false ->                               ?INVALID_BOARD_PARAMETER;
        is_boolean(Periodic)==false ->                              ?INVALID_BOARD_PARAMETER;
        (is_integer(Obstacles) or is_list(Obstacles)) ==false ->    ?INVALID_BOARD_PARAMETER;
        (is_integer(Jokers) or is_list(Jokers)) ==false ->          ?INVALID_BOARD_PARAMETER;
        true ->                         ok
    end;
validate_params(_Params) ->                                         ?INVALID_PARAMETER_CONFIG.
    
-spec db_to_state(#egGameType{}, #egGame{}) -> #state{}.
db_to_state ( #egGameType{params=Params}
            , #egGame{ gid=GameId
                     , tid=GameType
                     , players=Players
                     , bots=Bots
                     , etime=EndTime
                     , space=Space
                     , status=Status
                     , board=Board
                     , nmovers=NextMovers
                     , naliases=NextAliases
                     , nscores=NextScores
                     , moves=Moves
                     } 
            ) ->
    case validate_params(Params) of
        ok ->   
            #state  { gid=GameId
                    , tid=GameType
                    , width=maps:get(width, Params)
                    , height=maps:get(height, Params)
                    , run=maps:get(run, Params)
                    , gravity=maps:get(gravity, Params)
                    , periodic=maps:get(periodic, Params)
                    , players=Players
                    , bots=Bots
                    , etime=EndTime
                    , space=Space
                    , status=Status
                    , board=Board 
                    , nmovers=NextMovers
                    , naliases=NextAliases
                    , nscores=NextScores
                    , moves=Moves
                    };
        Error->
            Error
    end.

-spec state_to_db(#state{}, #egGame{}) -> #egGame{}.
% Merge engine state into the game record (read from the DB)
state_to_db ( #state{ etime=EndTime
                    , status=Status
                    , board=Board 
                    , nmovers=NextMovers
                    , naliases=NextAliases
                    , nscores=NextScores
                    , moves=Moves
                    }
            , Game
            ) ->
    Game#egGame{ etime=EndTime
               , status=Status
               , board=Board
               , nmovers=NextMovers
               , naliases=NextAliases
               , nscores=NextScores
               , moves=Moves
               }.

-spec save_state(#state{}) -> ok | egGameError().
save_state(State) -> egambo_game:write_game(state_to_db(State, egambo_game:read_game(State#state.gid))).

-spec prepare(#egGameType{}, #egGame{}) -> #egGame{}.
prepare( #egGameType{ players=2
                    , params=#{ width:=Width, height:=Height
                              , aliases:=Aliases, gravity:=Gravity
                              , obstacles:=Obstacles, jokers:=Jokers
                              } 
                    } % GameType
       , #egGame{players=[P1,P2]} = Game
       ) ->
    Board0 = list_to_binary(lists:duplicate(Width*Height,?AVAILABLE)),
    {ok, Board1} = case is_list(Obstacles) of
        true -> put_multi(Gravity, Board0, Width, cells_to_integer_list(Width, Height, Obstacles), ?OBSTACLE);
        _ ->    {ok, Board0}
    end, 
    {ok, Board2} = case is_list(Jokers) of
        true -> put_multi(Gravity, Board1, Width, cells_to_integer_list(Width, Height, Jokers), ?JOKER);
        _ ->    {ok, Board1}
    end,
    {ok, Board3} = case is_integer(Obstacles) of
        true -> put_random(Gravity, Board2, Width, Obstacles, ?OBSTACLE);
        _ ->    {ok, Board2}
    end, 
    {ok, B} = case is_integer(Jokers) of
        true -> put_random(Gravity, Board3, Width, Jokers, ?JOKER);
        _ ->    {ok, Board3}
    end,
    S = [0,0],                  % initial scores
    M = case random_idx1(2) of
        1 -> [P1,P2];
        2 -> [P2,P1]
    end,                        % initial movers (player AccountIds)
    Game#egGame{ialiases=Aliases, imovers=M, space=B, board=B, nmovers=M, naliases=Aliases, nscores=S}. 

-spec resume(egGameId()) -> ok | egGameError().
resume(GameId) ->
    ChildSpec = { ?ENGINE_ID(GameId)                                    % ChildId
                , {egambo_tictac, start_link, [GameId]}                 % {M,F,A}
                , temporary                                             % do not restart automatically
                , 1000                                                  % Shutdown timeout
                , worker                                                % Type
                , [egambo_tictac]                                       % Modules
                },
    supervisor:start_child(egambo_sup, ChildSpec).

-spec stop(egGameId()) -> ok | egGameError().
stop(GameId) ->
    supervisor:terminate_child(egambo_sup, ?ENGINE_ID(GameId)),
    supervisor:delete_child(egambo_sup, ?ENGINE_ID(GameId)).

result( #state{gid=GameId, status=Status, etime=EndTime, board=Board, nmovers=Movers, naliases=Aliases, nscores=Scores}) ->
    #{id=>GameId, etime=>EndTime, status=>Status, board=>Board, movers=>Movers, aliases=>Aliases, scores=>Scores};
result( #egGame{gid=GameId, status=Status, etime=EndTime, board=Board, nmovers=Movers, naliases=Aliases, nscores=Scores}) ->
    #{id=>GameId, etime=>EndTime, status=>Status, board=>Board, movers=>Movers, aliases=>Aliases, scores=>Scores};
result(GameId) -> gen_server:call(?GLOBAL_ID(GameId), result).

moves( #egGame{gid=GameId, status=Status, etime=EndTime, space=Space, moves=Moves}) ->
    #{id=>GameId, etime=>EndTime, status=>Status, space=>Space, moves=>Moves};
moves(GameId) -> gen_server:call(?GLOBAL_ID(GameId), moves).

start_link(GameId)  ->
    gen_server:start_link(?GLOBAL_ID(GameId), ?MODULE, [GameId], []).

state(GameId) ->
    gen_server:call(?GLOBAL_ID(GameId), state). 

print(GameId) ->
    gen_server:call(?GLOBAL_ID(GameId), print). 

play(GameId, Cell) -> gen_server:call(?GLOBAL_ID(GameId), {play, Cell}). 

play(GameId, Cell, Alias) -> gen_server:call(?GLOBAL_ID(GameId), {play, Cell, player_to_integer(Alias)}). 

play(GameId, Cell, Alias, MyAccountId) -> gen_server:call(?GLOBAL_ID(GameId), {play, Cell, player_to_integer(Alias), MyAccountId}). 

player_to_integer(Player) when is_integer(Player) -> Player; 
player_to_integer(Player) when is_atom(Player) -> hd(string:to_upper(atom_to_list(Player))); 
player_to_integer(undefined) -> undefined; 
player_to_integer("") -> undefined; 
player_to_integer(Player) when is_list(Player) -> hd(string:to_upper(Player)). 

cell_to_integer(Cell, Width, Height) when is_integer(Cell), Cell>=0, Cell<Width*Height -> Cell; 
cell_to_integer(Cell, _Width, _Height) when is_integer(Cell) -> ?INVALID_CELL; 
cell_to_integer(Cell, Width, Height) when is_atom(Cell) -> 
    C0 = hd(?COLS),
    R0 = hd(?ROWS),
    case atom_to_list(Cell) of
        [C] when C>=C0,C<C0+Width -> C-C0;
        [C,R] when C>=C0,C<C0+Width,R>=R0,R<R0+Height -> C-C0+Width*(R-R0);
        _ -> ?INVALID_CELL
    end. 

cells_to_integer_list(Width, Height, Cells) -> 
    cells_to_integer_list(Width, Height, Cells, []). 

cells_to_integer_list(_Width, _Height, [], Acc) -> lists:usort(Acc);
cells_to_integer_list(Width, Height, [Cell|Rest], Acc) -> 
    cells_to_integer_list(Width, Height, Rest, [cell_to_integer(Cell, Width, Height)|Acc]).

rotate([H|T]) -> T ++ [H].

init([GameId]) ->
    #egGame{status=Status} = Game = egambo_game:read_game(GameId),
    case Status of 
        playing ->
            GameType = egambo_game:read_type(Game#egGame.tid),
            case db_to_state(GameType, Game) of
                #state{nmovers=Movers, players=Players, bots=Bots, etime=EndTime} = State ->
                    print_board(State),                     % Todo: remove debugging
                    print_next(hd(State#state.naliases)),   % Todo: remove debugging
                    erlang:send_after(?AUTOSAVE_PERIOD, self(), {save_state, EndTime}),
                    invoke_bot_if_due(Movers, Players, Bots),
                    {ok, State};
                Error -> Error
            end;
        _ ->
            {stop, ?NOT_PLAYING}
    end.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_cast(_Request, State) -> {noreply, State}.

handle_info({play_bot, ?MODULE}, State) -> 
    % integrated simple and fast bot module, works with full state
    case play_bot(State) of
        {ok, Cell, NewBoard} ->   
            case handle_new_move(Cell, NewBoard, State) of
                {stop, normal, NewState} -> {stop, normal, NewState};
                {reply, ok, NewState} ->    {noreply, NewState}
            end;
        _ -> 
            {noreply, State}
    end;
handle_info({save_state, SavedEndTime}, #state{etime=EndTime} = State) ->
    case EndTime of
        SavedEndTime    -> ok;   
        _ ->            save_state(State)
    end,
    erlang:send_after(?AUTOSAVE_PERIOD, self(), {save_state, EndTime}),
    {noreply, State};
handle_info(_, State) -> {noreply, State}.

handle_call(state, _From, State) ->
    {reply, ok, State};
handle_call(print, _From, #state{naliases=[Player|_]} = State) ->
    print_board(State),
    print_next(Player),
    {reply, ok, State};
handle_call({play, _, _}, _From, #state{status=Status} = State) when Status /= playing ->
    {reply, ?NOT_PLAYING, State};
handle_call({play, _, _, _}, _From, #state{status=Status} = State) when Status /= playing ->
    {reply, ?NOT_PLAYING, State};
handle_call({play, Cell}, From, #state{naliases=[Player|_], nmovers=[AccountId|_]} = State) ->
    handle_call({play, Cell, Player, AccountId}, From, State);
handle_call({play, Cell, Player}, From, #state{naliases=[Player|_], nmovers=[AccountId|_]} = State) ->
    handle_call({play, Cell, Player, AccountId}, From, State);
handle_call({play, Cell, Player, AccountId} ,_From, #state{naliases=[Player|_], nmovers=[AccountId|_]} = State) ->
    #state{ width=Width, height=Height, gravity=Gravity, board=Board} = State,
    case cell_to_integer(Cell, Width, Height) of
        I when is_integer(I) ->
            case put(Gravity, Board, Width, I, Player) of
                {ok, Idx, NewBoard} ->  handle_new_move(Idx, NewBoard, State);
                ResultError ->          {reply, ResultError, State}   % e.g. already_occupied
            end;
        IdxError -> {reply, IdxError, State}
    end;
handle_call({play, _, _}, _From, State) ->
    {reply, ?NOT_YOUR_TURN, State};
handle_call({play, _, _, _}, _From, State) ->
    {reply, ?NOT_YOUR_TURN, State};
handle_call(result, _From, #state{gid=GameId, etime=EndTime, status=Status, board=Board, nmovers=Movers, naliases=Aliases, nscores=Scores} = State) ->  
    {reply, #{id=>GameId, etime=>EndTime, status=>Status, board=>Board, movers=>Movers, aliases=>Aliases, scores=>Scores}, State};
handle_call(moves, _From, #state{gid=GameId, etime=EndTime, status=Status, space=Space, moves=Moves} = State) ->  
    {reply, #{id=>GameId, etime=>EndTime, status=>Status, space=>Space, moves=>Moves}, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_new_move(Idx, NewBoard, #state{gid=GameId, width=Width, height=Height, run=Run, periodic=Periodic, naliases=Aliases, nmovers=Movers} = State) ->
    NewAliases = rotate(Aliases),
    NewMovers = rotate(Movers),
    NewMoves = [{hd(Aliases), Idx}|State#state.moves],
    NewTime = egambo_game:eg_time(),
    case is_win(NewBoard, Width, Height, Run, Periodic, Aliases) of
        true ->
            NewState = State#state{ etime=NewTime
                                  , status=finished
                                  , board=NewBoard
                                  , nmovers=NewMovers
                                  , naliases=NewAliases
                                  , nscores=[-1,1]
                                  , moves=NewMoves
                                  },
            save_state(NewState), 
            egambo_game:notify(NewTime, GameId, close, result(NewState)),
            print_board(NewState),      % ToDo: remove this debug stmt
            print_win(hd(Aliases)),     % ToDo: remove this debug stmt
            {stop, normal, NewState};
        false ->
            case is_tie(NewBoard, Width, Height, Run, Periodic, Aliases) of
                true ->
                    NewState = State#state{ etime=NewTime
                                          , status=finished
                                          , board=NewBoard
                                          , nmovers=NewMovers
                                          , naliases=NewAliases
                                          , nscores=[0,0]
                                          , moves=NewMoves
                                          },
                    save_state(NewState), 
                    egambo_game:notify(NewTime, GameId, close, result(NewState)),
                    print_board(NewState),  % ToDo: remove debug print
                    print_tie(),            % ToDo: remove debug print
                    {stop, normal, NewState};
                false ->
                    NewState = State#state{ etime=NewTime
                                          , board=NewBoard
                                          , nmovers=NewMovers
                                          , naliases=NewAliases
                                          , nscores=rotate(State#state.nscores)
                                          , moves=NewMoves
                                          },
                    egambo_game:notify(NewTime, GameId, close, result(NewState)),
                    print_board(NewState),      % ToDo: remove this debug stmt
                    print_next(hd(NewAliases)), % ToDo: remove this debug stmt
                    invoke_bot_if_due(NewMovers, NewState#state.players, NewState#state.bots),
                    {reply, ok, NewState}
            end
    end.

-spec invoke_bot_if_due(Movers::[egAccountId()], Players::[egAccountId()], Bots::[egBot()]) -> ok.
invoke_bot_if_due(_, _, [undefined, undefined]) -> ok;
invoke_bot_if_due([AccountId|_], Players, Bots) -> invoke_bot(AccountId, Players, Bots).

invoke_bot(_, [], []) -> ok;
invoke_bot(AccountId, [AccountId|_], [undefined|_]) -> ok;
invoke_bot(AccountId, [AccountId|_], [Module|_]) -> self() ! {play_bot, Module};
invoke_bot(AccountId, [_|Players], [_|Bots]) -> invoke_bot(AccountId, Players, Bots).

-spec put(boolean(), binary(), integer(), integer(), integer()) -> {ok, integer(), binary()} | {error, atom()}.
put(false, Board, _Width, Idx, NewVal) ->
    case binary:at(Board, Idx) of 
        ?AVAILABLE ->
            Prefix = binary:part(Board, 0, Idx),
            Suffix = binary:part(Board, Idx+1, size(Board)-Idx-1),
            {ok, Idx, <<Prefix/binary, NewVal:8, Suffix/binary>>};
        _ ->
            ?ALREADY_OCCUPIED
    end;
put(true, Board, Width, Idx, NewVal) ->
    gravity_put(Board, Width, NewVal, lists:seq(size(Board)-Width+(Idx rem Width), (Idx rem Width) , -Width)).

gravity_put(_, _, _, []) -> ?ALREADY_OCCUPIED;
gravity_put(Board, Width, NewVal, [Idx|Rest]) -> 
    case put(true, Board, Width, Idx, NewVal) of
        {ok, Idx, Binary} ->    {ok, Idx, Binary};
        ?ALREADY_OCCUPIED ->    gravity_put(Board, Width, NewVal, Rest)
    end.

-spec put_multi(boolean(), binary(), integer(), list(), integer()) -> {ok, binary()} | {error, atom()}.
put_multi(false, Board, _Width, [], _NewVal) -> {ok, Board};
put_multi(false, Board, Width, [Idx|Rest], NewVal) ->
    case put(false, Board, Width, Idx, NewVal) of     
        ?ALREADY_OCCUPIED ->         ?ALREADY_OCCUPIED;
        {ok, _, Binary} ->           put_multi(false, Binary, Width, Rest, NewVal)
    end;
put_multi(true, Board, _Width, [], _NewVal) -> {ok, Board};
put_multi(true, Board, Width, [Idx|Rest], NewVal) ->
    case put(true, Board, Width, Idx, NewVal) of
        ?ALREADY_OCCUPIED ->         ?ALREADY_OCCUPIED;
        {ok, _, Binary} ->           put_multi(true, Binary, Width, Rest, NewVal)
    end.

-spec put_random(boolean(), binary(), integer(), integer(), integer()) -> {ok, binary()} | {error, atom()}.
put_random(false, Board, _Width, 0, _) -> {ok, Board};
put_random(false, Board, Width, Count, NewVal) ->
    Idx = random_idx0(size(Board)),
    case put(false, Board, Width, Idx, NewVal) of 
        ?ALREADY_OCCUPIED ->         put_random(false, Board, Width, Count, NewVal);
        {ok, _, Binary} ->           put_random(false, Binary, Width, Count-1, NewVal)
    end;
put_random(true, Board, _Width, 0, _) -> {ok, Board};
put_random(true, Board, Width, Count, NewVal) ->
    Idx = random_idx0(Width),
    case put(true, Board, Width, Idx, NewVal) of 
        ?ALREADY_OCCUPIED ->         put_random(true, Board, Width, Count, NewVal);
        {ok, _, Binary} ->           put_random(true, Binary, Width, Count-1, NewVal)
    end.

random_idx0(Width) -> crypto:rand_uniform(0, Width). % 0..Width-1 / 0 based random integer

random_idx1(Length) -> crypto:rand_uniform(1, Length+1). % 1..Length / 1 based random integer

print_next(Next) -> ?Info("next move ~s",[[Next]]).

print_tie() ->
    ?Info("This game ended in a tie."),
    ?Info("").

print_win(Player) ->
    ?Info("Congratulations, ~s won",[[Player]]),
        ?Info("").

print_board(#state{width=Width} = State) -> 
    ?Info("~s",[""]),
    ?Info("board ~s",[" |" ++ lists:sublist(?COLS,Width) ++ "|"]),
    print_row(1, State).

print_row(N,#state{height=Height}) when N>Height -> ok;
print_row(N, State) -> 
    ?Info("      ~s",[row_str(N,State)]),
    print_row(N+1,State).

row_str(N,#state{width=Width, board=Board}) ->
    [lists:nth(N, ?ROWS),$|] ++ binary_to_list(binary:part(Board, (N-1) * Width, Width)) ++ "|".

-spec is_tie(Board::binary(), Width::integer(), Height::integer(), Run::integer(), Periodic::boolean(), Aliases::list()) -> boolean().
is_tie(Board, Width, Height, Run, Periodic, [Player|Other]) ->  
    case binary:match(Board, <<?AVAILABLE>>) of
        nomatch -> 
            true;
        _ -> case is_win(binary:replace(Board, <<?AVAILABLE:8>>, <<Player:8>>, [global]), Width, Height, Run, Periodic, [Player|Other]) of
                true -> 
                    false;
                false ->
                    not is_win(binary:replace(Board, <<?AVAILABLE:8>>, <<Other:8>>, [global]), Width, Height, Run, Periodic, Other)
            end
    end.

-spec is_win(Board::binary(), Width::integer(), Height::integer(), Run::integer(), Periodic::boolean(), Aliases::list()) -> boolean().
is_win(Board, Width, Height, Run, false, [Player|_]) -> 
    egambo_tictac_win:win(binary:replace(Board, <<?JOKER:8>>, <<Player:8>>, [global]), Width, Height, Run, Player);
is_win(Board, Width, Height, Run, true, [Player|_]) -> 
    egambo_tictac_wip:win(binary:replace(Board, <<?JOKER:8>>, <<Player:8>>, [global]), Width, Height, Run, Player).

-spec play_bot(#state{}) -> {ok, integer(), binary()} | {error, atom()}.
play_bot(#state{board=Board, width=Width, height=Height, gravity=Gravity} = State) ->
    Options = put_options(Board, Width, Height, Gravity),
    case play_bot_immediate_win(State, Options) of
        {ok, Idx, NewBoard} ->   
            {ok, Idx, NewBoard};   % win in this move detected
        _ ->
            case play_bot_defend_immediate(State, Options) of
                {ok, Idx, NewBoard} ->   
                    {ok, Idx, NewBoard};   % opponent's win in this move detected and taken
                _ ->
                    play_bot_random(State, Options)
            end
    end.

-spec play_bot_random(#state{}, Options::list()) -> {ok, Move::integer(), NewBoard::binary()} | {error, atom()}.
play_bot_random(#state{width=Width, naliases=[Player|_], board=Board, gravity=Gravity}, Options) ->
    put(Gravity, Board, Width, lists:nth(random_idx1(length(Options)), Options), Player).

-spec play_bot_immediate_win(#state{}, Options::list()) -> {ok, Move::integer(), NewBoard::binary()} | {nok, no_immediate_win} | {error, atom()}.
play_bot_immediate_win(_State, []) -> {nok, no_immediate_win};  
play_bot_immediate_win(#state{board=Board, width=Width, height=Height, run=Run, gravity=Gravity, periodic=Periodic, naliases=Aliases} = State, [I|Rest]) -> 
    {ok, Idx, TestBoard} = put(Gravity, Board, Width, I, hd(Aliases)),
    case is_win(TestBoard, Width, Height, Run, Periodic, Aliases) of
        true ->     {ok, Idx, TestBoard};
        false ->    play_bot_immediate_win(State, Rest)
    end.

-spec play_bot_defend_immediate(#state{}, Options::list()) -> {ok, Move::integer(), NewBoard::binary()} | {nok, no_immediate_risk} | {error, atom()}.
play_bot_defend_immediate(_State, []) -> {nok, no_immediate_risk};
play_bot_defend_immediate(#state{board=Board, width=Width, height=Height, run=Run, gravity=Gravity, periodic=Periodic, nmovers=[Player|Other]} = State, [Idx|Rest]) -> 
    {ok, Idx, TestBoard} = put(Gravity, Board, Width, Idx, hd(Other)),
    case is_win(TestBoard, Width, Height, Run, Periodic, Other) of
        true ->     put(Gravity, Board, Width, Idx, Player);
        false ->    play_bot_defend_immediate(State, Rest)
    end.

put_options(Board, Width, Height, false) ->
    lists:usort([ case B of ?AVAILABLE -> I; _ -> false end || {B,I} <- lists:zip(binary_to_list(Board), lists:seq(0,Width*Height-1))]) -- [false];
put_options(Board, Width, _Height, true) ->
    lists:usort([ case B of ?AVAILABLE -> I; _ -> false end || {B,I} <- lists:zip(binary_to_list(binary:part(Board,0,Width)), lists:seq(0,Width-1))]) -- [false].
