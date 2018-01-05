-module(egambo_tictac).

-include("egambo_tictac.hrl").  % import tictac game structures 

-behavior(gen_server).          % this is implemented on a gen_server basis
-behavior(egambo_gen_game).     % callbacks used by player (mediated through egambo_game)
-behavior(egambo_gen_engine).   % callbacks used by egambo_game (game manager)

-define(AUTOSAVE_PERIOD, 3000). % msec between state save to db

-define(NOT_YOUR_TURN, {error, not_your_turn}).
-define(NOT_PLAYING, {error, not_playing}).

-record(state,  { gid        :: egGameId()  % game id
                , tid        :: egGameTypeId()
                , width      :: integer()   % board width >= 3
                , height     :: integer()   % board height >= 3
                , run        :: integer()   % sucess run length
                , gravity    :: boolean()   % do moves fall towards higher row numbers
                , periodic   :: boolean()   % unbounded repeating board
                , winmod     :: egWinId()   % module name of win function
                , players  = []             :: [egAccountId()]     % hd(players) is owner (proposer) of the game 
                , bots     = []             :: [module()]          % internal bot player modules (undefined for external players)
                , etime    = undefined      :: egTime()            % last status change time (so far), game end time (eventually)
                , space    = <<>>           :: binary()            % initial board state
                , ialiases = []             :: [egAlias()]         % initial aliases (from game type parameters)
                , imovers  = []             :: [egAccountId()]     % initial player AccountId enumeration (first moves)
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
        , stop/1        % instruct gen_server to stop the game engine process (no attempt to save the state)
        ]).

% egambo_gen_game behavior callbacks (player originating requests)
-export([ play/2        % play one move in the name of the next player:         GameId, Cell
        , play/3        % play one move for the given AccountId:                GameId, Cell, AccountId
        , play/4        % play one move for given alis and AccountId:           GameId, Cell|Command, Alias, AccountId
        , result/1      % return current game state as a json-ready map:        GameId|#egGame{}
        , moves/1       % return game history as a json-ready map               GameId|#egGame{}
        , put/5         % changing one cell in the board (used by bots)
        , is_win/3      % checking the board for a win (used by bots)
        , is_tie/3      % checking the board for a tie (used by bots)
        , win_module/4  % module name of win function implementation for particular board parameters
        ]).

% debugging API
-export([ state/1
        , print/1
        ]).

-export([ norm_aliases/3        %% normalize board to initial alias order (simplifies bot playing) {}
        , norm_aliases_sym/7    %% normalize board to initial alias order and default symmetry {Input,Sym}
        , sample/5              %% sample one random move out of a finished game
        , samples/5             %% sample all moves out of a finished game
        , history/2             %% board with merged in move history
        , random_idx0/1         %% zero based integer random number
        , random_idx1/1         %% one based integer random number
        , put_options/4         
        , shuffle/1
        ]).

-safe([sample, samples, history, norm_aliases, norm_aliases_sym]).

win_module(Width, Height, Run, false) ->
    list_to_atom(atom_to_list(?MODULE) ++ lists:flatten(io_lib:format("_win_~p_~p_~p",[Width, Height, Run])));
win_module(Width, Height, Run, true) ->
    list_to_atom(atom_to_list(?MODULE) ++ lists:flatten(io_lib:format("_win_~p_~p_~p_p",[Width, Height, Run]))).

-spec validate_params(egTicTacParams()) -> ok | egGameError().
validate_params(#{width:=Width, height:=Height, run:=Run, gravity:=Gravity, periodic:=Periodic, obstacles:=Obstacles, jokers:=Jokers, aliases:=Aliases}) -> 
    MaxDim = max(Width, Height),
    if
        is_list(Aliases) == false ->                                ?INVALID_ALIAS_PARAMETER;
        length(Aliases) /= 2 ->                                     ?INVALID_ALIAS_PARAMETER;
        is_integer(hd(Aliases)) == false ->                         ?INVALID_ALIAS_PARAMETER;
        is_integer(hd(tl(Aliases))) == false ->                     ?INVALID_ALIAS_PARAMETER;
        is_integer(Width)==false ->                                 ?INVALID_WIDTH_PARAMETER;
        (Width<3) or (Width>?MAX_DIMENSION) ->                      ?INVALID_WIDTH_PARAMETER;
        is_integer(Height)==false ->                                ?INVALID_HEIGHT_PARAMETER;
        (Height<3) or (Height>?MAX_DIMENSION) ->                    ?INVALID_HEIGHT_PARAMETER;
        is_integer(Run)==false ->                                   ?INVALID_RUN_PARAMETER;
        (Run<3) or (Run>MaxDim) ->                                  ?INVALID_RUN_PARAMETER;
        is_boolean(Gravity)==false ->                               ?INVALID_BOARD_PARAMETER;
        is_boolean(Periodic)==false ->                              ?INVALID_BOARD_PARAMETER;
        (is_integer(Obstacles) or is_list(Obstacles)) ==false ->    ?INVALID_BOARD_PARAMETER;
        (is_integer(Jokers) or is_list(Jokers)) ==false ->          ?INVALID_BOARD_PARAMETER;
        true ->
            try  
                case lists:member({win,2}, apply(win_module(Width, Height, Run, Periodic), module_info, [exports])) of
                    true ->         ok;
                    false ->        ?MISSING_WIN_FUNCTION
                end
            catch {error,undef} ->  ?MISSING_WIN_MODULE
            end
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
                     , ialiases=IAliases
                     , imovers=IMovers
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
            Width=maps:get(width, Params),
            Height=maps:get(height, Params),
            Run=maps:get(run, Params),
            Gravity=maps:get(gravity, Params),
            Periodic=maps:get(periodic, Params),
            #state  { gid=GameId
                    , tid=GameType
                    , width=Width
                    , height=Height
                    , run=Run
                    , gravity=Gravity
                    , periodic=Periodic
                    , winmod=win_module(Width, Height, Run, Periodic) 
                    , players=Players
                    , bots=Bots
                    , etime=EndTime
                    , space=Space
                    , ialiases=IAliases
                    , imovers=IMovers
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
    case egambo_tictac_sup:start_game(GameId) of
        {ok,_} ->                      ok;
        {error,{already_started,_}} -> ok;
        Error ->                       Error
    end.

-spec stop(egGameId()) -> ok | egGameError().
stop(GameId) ->
    gen_server:call(?ENGINE_GID(GameId), stop).

result( #state{gid=GameId, width=Width, height=Height, run=Run, gravity=Gravity, periodic=Periodic, status=Status, etime=EndTime, board=Board, nmovers=Movers, naliases=Aliases, nscores=Scores}) ->
    #{id=>GameId, etime=>?EG_SEC(EndTime), width=>Width, height=>Height, run=>Run, gravity=>Gravity, periodic=>Periodic, status=>Status, board=>Board, movers=>Movers, aliases=>Aliases, scores=>Scores};
result( #egGame{gid=GameId, tid=GameTypeId, status=Status, etime=EndTime, board=Board, nmovers=Movers, naliases=Aliases, nscores=Scores}) ->
    case egambo_game:read_type(GameTypeId) of
        #egGameType{params=#{width:=Width, height:=Height, run:=Run, gravity:=Gravity, periodic:=Periodic}} ->
            #{id=>GameId, etime=>?EG_SEC(EndTime), width=>Width, height=>Height, run=>Run, gravity=>Gravity, periodic=>Periodic, status=>Status, board=>Board, movers=>Movers, aliases=>Aliases, scores=>Scores};
        Error ->
            Error
    end;   
result(GameId) -> gen_server:call(?ENGINE_GID(GameId), result).

moves( #egGame{gid=GameId, status=Status, etime=EndTime, space=Space, moves=Moves}) ->
    #{id=>GameId, etime=>?EG_SEC(EndTime), status=>Status, space=>Space, moves=>lists:reverse(Moves)};
moves(GameId) -> gen_server:call(?ENGINE_GID(GameId), moves).

start_link(GameId)  ->
    gen_server:start_link(?ENGINE_GID(GameId), ?MODULE, [GameId], []).

state(GameId) ->
    gen_server:call(?ENGINE_GID(GameId), state). 

print(GameId) ->
    gen_server:call(?ENGINE_GID(GameId), print). 

play(GameId, Cell) -> gen_server:call(?ENGINE_GID(GameId), {play, Cell}). 

play(GameId, Cell, AccountId) -> gen_server:call(?ENGINE_GID(GameId), {play, Cell, AccountId}). 

play(GameId, Cell, Alias, MyAccountId) -> gen_server:call(?ENGINE_GID(GameId), {play, Cell, player_to_integer(Alias), MyAccountId}). 

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
        [C] when C>=C0, C<C0+Width -> C-C0;
        [C,R] when C>=C0, C<C0+Width, R>=R0, R<R0+Height -> C-C0+Width*(R-R0);
        _ -> ?INVALID_CELL
    end.

cells_to_integer_list(Width, Height, Cells) -> 
    cells_to_integer_list(Width, Height, Cells, []). 

cells_to_integer_list(_Width, _Height, [], Acc) -> lists:usort(Acc);
cells_to_integer_list(Width, Height, [Cell|Rest], Acc) -> 
    cells_to_integer_list(Width, Height, Rest, [cell_to_integer(Cell, Width, Height)|Acc]).

% External helper functions

history(Space, Moves) -> 
    Hboard = [ list_to_binary([Ch]) || Ch <- binary_to_list(Space)],
    L = length(Moves),
    Fun = fun({I, {Alias, Pos}}, HAcc) -> 
              {B1,B2} = lists:split(Pos, HAcc),
              B1 ++ [list_to_binary([Alias|integer_to_list(I)])] ++ tl(B2) 
          end,
    lists:foldl(Fun, Hboard, lists:zip(lists:seq(1,L),lists:reverse(Moves))).

-spec sample(binary(), [egAlias()], [egGameMove()], [egAlias()], [egScore()]) -> list().
%% sample one random move out of a finished game, aliases and scores normalized to initial move order
%% input:
%% Space::binary() game board before the first move, often all spaces, sometimes with jokers $* or obstacles $$
%% IAliases::[integer()] initial aliases, list of integers, often [88,79] = [$X,$O] = "XO", asc(X) being the first player alias 
%% Moves::[egGameMove()] reversed list of moves for whole game (first move is last element of the list)
%% FAliases::[integer()] final aliases, element positions matching FScores  
%% FScores::[float()] final scores, list of floats, positions matching FAliases 
%% output (as a list):
%% Board::string() board before the move, normalized to player hd(IAliases) playing the move
%% Players::string() players before the move hd(Players)=PlayerDoingTheMove (normally called $X)
%% Move::integer()  move played (index into board)
%% Scores::[float()] score of players after finishing the game hd(Scores)=ScoreOfPlayerDoingTheMove
%% MTE::integer() moves to end, number of moves played from this move to the end of the game, 0 if this move is the winning one
sample(Space, IAliases, Moves, FAliases, FScores) ->
    L = length(Moves),                                  % number of moves in this game
    MTE = random_idx0(L),                               % number of (end) moves (0..L-1) to throw away
    sample_move(Space, IAliases, Moves, FAliases, FScores, MTE).

sample_move(Space, IAliases, Moves, FAliases, FScores, MTE) ->
    {_, UsedMoves} = lists:split(MTE, Moves),             % used moves to construct the board (others thrown away)
    sample_board(Space, IAliases, lists:reverse(UsedMoves), FAliases, FScores, MTE).

sample_board(Board, [X|IAliases], [{X,Move}], [X|_], FScores, MTE) when is_binary(Board) ->
    % Player matches initial player and that player's final score 
    [binary_to_list(Board), [X|IAliases], Move, FScores, MTE];
sample_board(Board, [X|IAliases], [{X,Move}], [X|_], FScores, MTE) ->
    % Player matches initial player and that player's final score 
    [Board, [X|IAliases], Move, FScores, MTE];
sample_board(Board, [X|IAliases], [{X,Move}], FAliases, FScores, MTE) ->
    % Player matches initial player but final score needs to be rotated 
    sample_board(Board, [X|IAliases], [{X,Move}], rotate(FAliases), rotate(FScores), MTE);
sample_board(Board, [X,O|_] = IAliases, [{O,Move}], FAliases, FScores, MTE) ->
    % Player does not match initial player. Board normalisation needed.  
    RotatedBoard = norm_aliases(Board, rotate(IAliases), IAliases), 
    sample_board(RotatedBoard, IAliases, [{X,Move}], rotate(FAliases), FScores, MTE);
sample_board(Board, IAliases, [{P,Move}|Moves], FAliases, FScores, MTE) ->
    % One more move to aggregate into the board. 
    {ok, _, NewBoard} = put(false, Board, 0, Move, P),
    sample_board(NewBoard, IAliases, Moves, FAliases, FScores, MTE).

-spec samples(binary(), [egAlias()], [egGameMove()], [egAlias()], [egScore()]) -> list().
%% sample all moves out of a finished game, aliases and scores normalized to initial move order
%% input: see sample
%% output (as a list): list of samples, see sample
samples(Space, IAliases, Moves, FAliases, FScores) ->
    [sample_move(Space, IAliases, Moves, FAliases, FScores, MTE) || MTE <- lists:seq(0,length(Moves)-1)].

-spec norm_aliases_sym(binary() | list(), [egAlias()], [egAlias()], integer(), integer(), boolean(), boolean()) -> {list(), atom()}.
norm_aliases_sym(Board, NAliases, IAliases, Width, Height, Gravity, Periodic) ->
    egambo_tictac_sym:norm(Width, Height, Gravity, Periodic, norm_aliases(Board, NAliases, IAliases)).

-spec norm_aliases(binary() | list(), [egAlias()], [egAlias()]) -> list().
%% transform a tictac board by swapping player aliases to a given next player
%% used to prepare th board for bots whinorm_aliases(Board, Aliases, Aliases) ch prefer to always play as X
%% can also be used to transform the returned NewBoard back to the real player alias
norm_aliases(Board, Aliases, Aliases) when is_binary(Board) -> binary_to_list(Board);
norm_aliases(Board, Aliases, Aliases) -> Board;
norm_aliases(Board, FromAliases, ToAliases) when is_binary(Board) -> 
    [flip(P, FromAliases, ToAliases) || P <- binary_to_list(Board)];
norm_aliases(Board, FromAliases, ToAliases)  -> 
    [flip(P, FromAliases, ToAliases) || P <- Board].

put_options(Board, Width, Height, false) ->
    shuffle(lists:usort([ case B of ?AVAILABLE -> I; _ -> false end || {B,I} <- lists:zip(binary_to_list(Board), lists:seq(0,Width*Height-1))]) -- [false]);
put_options(Board, Width, _Height, true) ->
    shuffle(lists:usort([ case B of ?AVAILABLE -> I; _ -> false end || {B,I} <- lists:zip(binary_to_list(binary:part(Board,0,Width)), lists:seq(0,Width-1))]) -- [false]).

shuffle(L) -> [ X || {_,X} <- lists:sort([{rand:uniform(), N} || N <- L])].

flip(P, [], []) -> P;
flip(P, [P|_], [I|_]) -> I;
flip(P, [_|FromAliases], [_|ToAliases]) -> flip(P, FromAliases, ToAliases).

rotate([H|T]) -> T ++ [H].

init([GameId]) ->
    #egGame{status=Status} = Game = egambo_game:read_game(GameId),
    case Status of 
        playing ->
            GameType = egambo_game:read_type(Game#egGame.tid),
            case db_to_state(GameType, Game) of
                #state{nmovers=Movers, players=Players, bots=Bots, etime=EndTime} = State ->
                    erlang:send_after(?AUTOSAVE_PERIOD, self(), {save_state, EndTime}),
                    invoke_bot_if_due(Movers, Players, Bots),
                    {ok, State};
                Error -> 
                    Error
            end;
        _ ->
            {stop, ?NOT_PLAYING}
    end.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_cast(_Request, State) -> {noreply, State}.

handle_info({play_bot_req, BotId}, #state{gid=GameId, tid=GameTypeId, board=Board, naliases=Aliases} = State) -> 
    egambo_game:play_bot(BotId, GameTypeId, GameId, Board, Aliases),
    {noreply, State};
handle_info({play_bot_resp, Player, {ok, Cell, NewBoard}}, #state{naliases=[Player|_]} = State) ->
    case handle_new_move(Cell, NewBoard, State) of
        {stop, normal, NewState} -> {stop, normal, NewState};
        {reply, ok, NewState} ->    {noreply, NewState}
    end;
handle_info({play_bot_resp, Player, Error}, #state{tid=GameTypeId, gid=GameId, bots=Bots, naliases=[Player|_], players=Players} = State) ->
    NewTime = egambo_game:eg_time(),
    egambo_game:notify(NewTime, GameTypeId, GameId, error, {play_bot_resp, Player, Error}, Bots, Players),
    {noreply, State};
handle_info({save_state, SavedEndTime}, #state{etime=EndTime} = State) ->
    case EndTime of
        SavedEndTime    -> ok;   
        _ ->            save_state(State)
    end,
    erlang:send_after(?AUTOSAVE_PERIOD, self(), {save_state, EndTime}),
    {noreply, State};
handle_info(_, State) -> {noreply, State}.

handle_call(state, _From, State) ->
    {reply, State, State};
handle_call(print, _From, #state{status=Status, naliases=[Player|Others], nscores=Scores} = State) ->
    print_board(State),
    case Status of
        playing -> print_next(Player);
        finished when Scores == [0,0] ->    print_tie();
        finished when Scores == [-1,1] ->   print_win(hd(Others));
        _ ->                                print_status(Status)
    end,
    {reply, ok, State};
handle_call({play, _, _}, _From, #state{status=Status} = State) when Status /= playing ->
    {reply, ?NOT_PLAYING, State};
handle_call({play, _, _, _}, _From, #state{status=Status} = State) when Status /= playing ->
    {reply, ?NOT_PLAYING, State};
handle_call({play, Cell}, From, #state{naliases=[Player|_], nmovers=[AccountId|_]} = State) ->
    handle_call({play, Cell, Player, AccountId}, From, State);
handle_call({play, Cell, AccountId}, From, #state{naliases=[Player|_], nmovers=[AccountId|_]} = State) ->
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
handle_call(result, _From, #state{gid=GameId, etime=EndTime, width=Width, height=Height, run=Run, gravity=Gravity, periodic=Periodic, status=Status, board=Board, nmovers=Movers, naliases=Aliases, nscores=Scores} = State) ->  
    {reply, #{id=>GameId, etime=>EndTime, width=>Width, height=>Height, run=>Run, gravity=>Gravity, periodic=>Periodic, status=>Status, board=>Board, movers=>Movers, aliases=>Aliases, scores=>Scores}, State};
handle_call(moves, _From, #state{gid=GameId, etime=EndTime, status=Status, space=Space, moves=Moves} = State) ->  
    {reply, #{id=>GameId, etime=>EndTime, status=>Status, space=>Space, moves=>lists:reverse(Moves)}, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_new_move(Idx, NewBoard, #state{tid=GameTypeId, gid=GameId, ialiases=IAliases, imovers=IMovers, space=Space, naliases=Aliases, nmovers=Movers, bots=Bots, winmod=WinMod, players=Players} = State) ->
    NewAliases = rotate(Aliases),
    NewMovers = rotate(Movers),
    NewMoves = [{hd(Aliases), Idx}|State#state.moves],
    NewTime = egambo_game:eg_time(),
    case is_win(WinMod, NewBoard, Aliases) of
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
            egambo_game:notify(NewTime, GameTypeId, GameId, close, result(NewState), Bots, Players),
            notify_bots(Bots, GameTypeId, GameId, IAliases, IMovers, Space, finished, NewMovers, NewAliases, [-1,1], NewMoves),
            {stop, normal, NewState};
        false ->
            case is_tie(WinMod, NewBoard, Aliases) of
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
                    egambo_game:notify(NewTime, GameTypeId, GameId, close, result(NewState), Bots, Players),
                    notify_bots(Bots, GameTypeId, GameId, IAliases, IMovers, Space, finished, NewMovers, NewAliases, [0,0], NewMoves),
                    {stop, normal, NewState};
                false ->
                    NewState = State#state{ etime=NewTime
                                          , board=NewBoard
                                          , nmovers=NewMovers
                                          , naliases=NewAliases
                                          , nscores=rotate(State#state.nscores)
                                          , moves=NewMoves
                                          },
                    egambo_game:notify(NewTime, GameTypeId, GameId, status, result(NewState), Bots, Players),
                    invoke_bot_if_due(NewMovers, NewState#state.players, NewState#state.bots),
                    {reply, ok, NewState}
            end
    end.

-spec invoke_bot_if_due(Movers::[egAccountId()], Players::[egAccountId()], Bots::[egBotId()]) -> ok.
invoke_bot_if_due(_, _, [undefined, undefined]) -> ok;
invoke_bot_if_due([AccountId|_], Players, Bots) -> invoke_bot(AccountId, Players, Bots).

invoke_bot(_, [], []) -> ok;
invoke_bot(AccountId, [AccountId|_], [undefined|_]) -> ok;
invoke_bot(AccountId, [AccountId|_], [Module|_]) -> self() ! {play_bot_req, Module};
invoke_bot(AccountId, [_|Players], [_|Bots]) -> invoke_bot(AccountId, Players, Bots).

notify_bots(Bots, GameTypeId, GameId, IAliases, _IMovers, Space, Status, _NMovers, NAliases, NScores, Moves) ->
    [notify_bot(BotId, GameTypeId, GameId, IAliases, Space, Status, NAliases, NScores, Moves) || BotId <- lists:usort(Bots)].

-spec notify_bot(egBotId(), egGameTypeId(), egGameId(), [egAlias()], binary(), egGameStatus(), [egAlias()], [egScore()], [{egAlias(),integer()}]) -> ok | {error, atom()}.
notify_bot(undefined, _, _, _, _, _, _, _, _) -> ok;
notify_bot(BotId, GameTypeId, GameId, IAliases, Space, Status, NAliases, NScores, Moves) ->
    global:send(?BOT_ID(BotId, GameTypeId), {notify_bot_req, GameId, IAliases, Space, Status, NAliases, NScores, Moves}).

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
    case put(false, Board, Width, Idx, NewVal) of
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

random_idx0(Size) -> rand:uniform(Size)-1.      % 0..Size-1 / 0 based random integer pointing into a binary

random_idx1(Length) -> rand:uniform(Length).    % 1..Length / 1 based random integer pointing into a list

print_next(Next) -> ?Info("next move ~s",[[Next]]).

print_tie() ->
    ?Info("This game ended in a tie."),
    ?Info("").

print_win(Player) ->
    ?Info("Congratulations, ~s won",[[Player]]),
        ?Info("").

print_status(Status) ->
    ?Info("Game Status, ~p",[Status]),
        ?Info("").

print_board(#state{gid=GameId, width=Width} = State) -> 
    ?Info("~s",[""]),
    ?Info("Board ~s ~p",[" |" ++ lists:sublist(?COLS,Width) ++ "|", GameId]),
    print_row(1, State).

print_row(N,#state{height=Height}) when N>Height -> ok;
print_row(N, State) -> 
    ?Info("      ~s",[row_str(N,State)]),
    print_row(N+1,State).

row_str(N,#state{width=Width, board=Board}) ->
    [lists:nth(N, ?ROWS),$|] ++ binary_to_list(binary:part(Board, (N-1) * Width, Width)) ++ "|".

-spec is_win(egWinId(), Board::binary(), Aliases::list()) -> boolean().
is_win(WinMod, Board, [Player|_]) -> 
    WinMod:win(binary:replace(Board, <<?JOKER:8>>, <<Player:8>>, [global]), Player).

-spec is_tie(egWinId(), Board::binary(), Aliases::list()) -> boolean().
is_tie(WinMod, Board, [Player|Others]) ->  
    case binary:match(Board, <<?AVAILABLE>>) of
        nomatch -> 
            true;
        _ -> case is_win(WinMod, binary:replace(Board, <<?AVAILABLE:8>>, <<Player:8>>, [global]), [Player|Others]) of
                true -> 
                    false;
                false ->
                    Other = hd(Others),
                    not is_win(WinMod, binary:replace(Board, <<?AVAILABLE:8>>, <<Other:8>>, [global]), Others)
            end
    end.
