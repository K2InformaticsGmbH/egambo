-module(egambo_tictac).

% -include("egambo.hrl").         % import logging macros
-include("egambo_game.hrl").    % import game managing structures 

-behavior(gen_server).          % 
-behavior(egambo_gen_game).     % implicitly defines the required callbacks

-define(COLS, "abcdefgh").
-define(ROWS, "12345678").
-define(OBSTACLE, $$).
-define(JOKER, $*).
-define(AVAILABLE, 32).         % space

-type egTicTacParams() ::  #{ width =>integer()
                            , height => integer()
                            , run => integer()
                            , gravity => boolean()
                            , periodic => boolean()
                            , obstacles => [integer()] | integer()
                            , jokers => [integer()] | integer()
                            , aliases => [integer()]
                            }.

-record(state,
                { width      :: integer()   % board width >= 3
                , height     :: integer()   % board height >= 3
                , run        :: integer()   % sucess run length
                , starter    :: integer()   % starting player (capital ascii)
                , other      :: integer()   % other player (capital ascii)
                , starterBot :: atom()      % starter player bot module (or undefined) 
                , otherBot   :: atom()      % other player bot module (or undefined) 
                , gravity    :: boolean()   % do moves fall towards higher row numbers
                , periodic   :: boolean()   % unbounded repeating board 
                , board      :: binary()    % 
                , next       :: integer()   % Starter|Other
                }
       ).

% gen_server behavior callback exports

-export([ start_link/1
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

% egambo_gen_game behavior callback exports
-export([ preset/3
        , play/4
        , forfeit/3
        , status/3
        , stop/0
        ]).

% debugging API
-export([ start/11
        , play/1
        , play/2
        , state/0
        , print/0
        ]).

% sample usage for demo:
% plain tic-tac-toe
% egambo_tictac:start(3,3,3,0,0,"X","O",undefined,false,false).
% 4x4 tic-tac-toe against bot "O"
% egambo_tictac:start(4,4,4,1,1,"X","O","O",false,false).
% plain four wins
% egambo_tictac:start(7,6,4,0,0,"X","O","O",false,false).
% egambo_tictac:play(a1).
% egambo_tictac:stop().

preset(  #egGameType{ players=2
                    , params=#{ width:=Width, height:=Height
                              , aliases:=Aliases
                              , gravity:=Gravity
                              , obstacles:=Obstacles, jokers:=Jokers
                              } 
                    } % GameType
       , #egGame{players=[P1,P2]} = Game
       , _Opts ) ->
    Board0 = list_to_binary(lists:duplicate(Width*Height,?AVAILABLE)),
    {ok, Board1} = case {Gravity, is_list(Obstacles)} of
        {false,true} -> put_multi(Board0, cells_to_integer_list(Width, Height, Obstacles), ?OBSTACLE);
        {true,true} ->  gravity_put_multi(Board0, Width, cells_to_integer_list(Width, Height, Obstacles), ?OBSTACLE);
        _ ->            {ok, Board0}
    end, 
    {ok, Board2} = case {Gravity, is_list(Jokers)} of
        {false,true} -> put_multi(Board1, cells_to_integer_list(Width, Height, Jokers), ?JOKER);
        {true,true} ->  gravity_put_multi(Board1, Width, cells_to_integer_list(Width, Height, Jokers), ?JOKER);
        _ ->            {ok, Board1}
    end,
    {ok, Board3} = case {Gravity, is_integer(Obstacles)} of
        {false,true} -> put_random(Board2, Obstacles, ?OBSTACLE);
        {true,true} ->  gravity_put_random(Board2, Width, Obstacles, ?OBSTACLE);
        _ ->            {ok, Board2}
    end, 
    {ok, B} = case {Gravity, is_integer(Jokers)} of
        {false,true} -> put_random(Board3, Jokers, ?JOKER);
        {true,true} ->  gravity_put_random(Board3, Width, Jokers, ?JOKER);
        _ ->            {ok, Board3}
    end,
    S = [0,0],                  % initial scores
    M = case random_idx1(2) of
        1 -> [P1,P2];
        2 -> [P2,P1]
    end,                        % initial movers (player AccountIds)
    Game#egGame{ialiases=Aliases, imovers=M, preset=B, board=B, nmovers=M, naliases=Aliases, nscores=S}. 

play(_GameId, _Move, _Opts, _MyAccountId) -> ?egGameNotImplemented.

forfeit(_GameId, _Opts, _MyAccountId) -> ?egGameNotImplemented.

status(_GameId, _Opts, _MyAccountId) -> ?egGameNotImplemented.

start(Width, Height, Run, Obstacles, Jokers, Starter, Other, StarterBot, OtherBot, Gravity, Periodic) ->
    Board0 = list_to_binary(lists:duplicate(Width*Height,?AVAILABLE)),
    {ok, Board1} = case {Gravity, is_list(Obstacles)} of
        {false,true} -> put_multi(Board0, cells_to_integer_list(Width, Height, Obstacles), ?OBSTACLE);
        {true,true} ->  gravity_put_multi(Board0, Width, cells_to_integer_list(Width, Height, Obstacles), ?OBSTACLE);
        _ ->            {ok, Board0}
    end, 
    {ok, Board2} = case {Gravity, is_list(Jokers)} of
        {false,true} -> put_multi(Board1, cells_to_integer_list(Width, Height, Jokers), ?JOKER);
        {true,true} ->  gravity_put_multi(Board1, Width, cells_to_integer_list(Width, Height, Jokers), ?JOKER);
        _ ->            {ok, Board1}
    end,
    {ok, Board3} = case {Gravity, is_integer(Obstacles)} of
        {false,true} -> put_random(Board2, Obstacles, ?OBSTACLE);
        {true,true} ->  gravity_put_random(Board2, Width, Obstacles, ?OBSTACLE);
        _ ->            {ok, Board2}
    end, 
    {ok, Board4} = case {Gravity, is_integer(Jokers)} of
        {false,true} -> put_random(Board3, Jokers, ?JOKER);
        {true,true} ->  gravity_put_random(Board3, Width, Jokers, ?JOKER);
        _ ->            {ok, Board3}
    end, 
    Params = [ Width
             , Height
             , Run
             , player_to_integer(Starter)
             , player_to_integer(Other)
             , StarterBot
             , OtherBot
             , Gravity
             , Periodic
             , Board4
             ],
    ChildSpec = { egambo_tictac                          % ChildId
                , {egambo_tictac,start_link,[Params]}    % {M,F,A}
                , permanent                           % Restart strategy
                , 1000                                % Shutdown timeout
                , worker                              % Type
                , [egambo_tictac]                        % Modules
                },
    supervisor:start_child(egambo_sup, ChildSpec).

stop() ->
    % ToDo: save game state to the database
    supervisor:terminate_child(egambo_sup, egambo_tictac),
    supervisor:delete_child(egambo_sup, egambo_tictac).

start_link(Params)  ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).

state() ->
    gen_server:call(?MODULE, state). 

print() ->
    gen_server:call(?MODULE, print). 

play(Cell) -> gen_server:call(?MODULE, {play, Cell}). 

play(Player, Cell) -> gen_server:call(?MODULE, {play, player_to_integer(Player), Cell}). 

player_to_integer(Player) when is_integer(Player) -> Player; 
player_to_integer(Player) when is_atom(Player) -> hd(string:to_upper(atom_to_list(Player))); 
player_to_integer(undefined) -> undefined; 
player_to_integer("") -> undefined; 
player_to_integer(Player) when is_list(Player) -> hd(string:to_upper(Player)). 

cell_to_integer(Cell, Width, Height) when is_integer(Cell), Cell>=0, Cell<Width*Height -> Cell; 
cell_to_integer(Cell, _Width, _Height) when is_integer(Cell) -> {error, invalid_cell}; 
cell_to_integer(Cell, Width, Height) when is_atom(Cell) -> 
    C0 = hd(?COLS),
    R0 = hd(?ROWS),
    case atom_to_list(Cell) of
        [C] when C>=C0,C<C0+Width -> C-C0;
        [C,R] when C>=C0,C<C0+Width,R>=R0,R<R0+Height -> C-C0+Width*(R-R0);
        _ -> {error, invalid_cell}
    end. 

cells_to_integer_list(Width, Height, Cells) -> 
    cells_to_integer_list(Width, Height, Cells, []). 

cells_to_integer_list(_Width, _Height, [], Acc) -> lists:usort(Acc);
cells_to_integer_list(Width, Height, [Cell|Rest], Acc) -> 
    cells_to_integer_list(Width, Height, Rest, [cell_to_integer(Cell, Width, Height)|Acc]).

init([Width, Height, Run, Starter, Other, StarterBot, OtherBot, Gravity, Periodic, Board]) ->
    State = #state  { width=Width
                    , height=Height
                    , run=Run
                    , starter=Starter
                    , other=Other
                    , starterBot=StarterBot
                    , otherBot=OtherBot
                    , gravity=Gravity
                    , periodic=Periodic
                    , board=Board 
                    , next=Starter
                    },
    print_board(State),
    print_next(Starter),
    invoke_bot_if_due(Starter, Starter, StarterBot, Other, OtherBot),
    {ok, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_cast(_Request, State) -> {noreply, State}.

handle_info({play_bot, Player}, #state{ width=Width, height=Height, run=Run, next=Player
                                      , starter=Starter, other=Other, starterBot=StarterBot, otherBot=OtherBot
                                      , periodic=Periodic} = State) -> 
    case play_bot(State) of
        {ok, NewBoard} ->   
            Next = next_player(Player, Starter, Other),
            NewState = State#state{next=Next, board=NewBoard},
            case is_win(NewBoard, Width, Height, Run, Periodic, Player) of
                true -> 
                    print_board(NewState),
                    print_win(Player),
                    {stop, normal, NewState};
                false ->
                    print_board(NewState),
                    case is_tie(NewBoard, Width, Height, Run, Periodic, Player, Next) of
                        true ->
                            print_tie(),
                            {stop, normal, NewState};
                        false ->
                            print_next(Next),
                            invoke_bot_if_due(Next, Starter, StarterBot, Other, OtherBot),    
                            {noreply, NewState}
                    end
            end;
        _ -> 
            {noreply, State}
    end;
handle_info(_, State) -> {noreply, State}.

handle_call(state, _From, State) ->
    ?Info("state ~p",[State]),
    {reply, ok, State};
handle_call(print, _From, #state{next=Player} = State) ->
    print_board(State),
    print_next(Player),
    {reply, ok, State};
handle_call({play, Cell}, _From, #state{next=Player} = State) ->
    handle_call({play, Player, Cell}, _From, State);
handle_call({play, Player, Cell}, _From, #state{ width=Width, height=Height, run=Run, next=Player
                                               , starter=Starter, other=Other, starterBot=StarterBot, otherBot=OtherBot
                                               , board=Board, gravity=Gravity, periodic=Periodic} = State) ->
    case cell_to_integer(Cell, Width, Height) of
        {error, invalid_cell} -> {reply, invalid_cell, State};
        Idx when is_integer(Idx) ->
            Result = case Gravity of
                false ->    put(Board, Idx, Player);
                true ->     gravity_put(Board, Width, Idx, Player) 
            end,
            case Result of
                {error, already_occupied} -> {reply, already_occupied, State};
                {ok, NewBoard} ->   
                    Next = next_player(Player, Starter, Other),
                    NewState = State#state{next=Next, board=NewBoard},
                    case is_win(NewBoard, Width, Height, Run, Periodic, Player) of
                        true -> 
                            print_board(NewState),
                            print_win(Player),
                            {stop, normal, NewState};
                        false ->
                            print_board(NewState),
                            case is_tie(NewBoard, Width, Height, Run, Periodic, Player, Next) of
                                true ->
                                    print_tie(),
                                    {stop, normal, NewState};
                                false ->
                                    print_next(Next),
                                    invoke_bot_if_due(Next, Starter, StarterBot, Other, OtherBot),
                                    {reply, ok, NewState}
                            end
                    end
            end
    end;
handle_call({play, _, _}, _From, State) ->
    {reply, not_your_turn, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

invoke_bot_if_due(Next, Next, undefined, _, _) -> ok;
invoke_bot_if_due(Next, _, _, Next, undefined) -> ok;
invoke_bot_if_due(Next, _, _, _, _) -> self() ! {play_bot, Next}.

-spec gravity_put(binary(), integer(), integer(), integer()) -> {ok,binary()} | {error,atom()}.
gravity_put(Board, Width, Idx, NewVal) ->
    gravity_put(Board, NewVal, lists:seq(size(Board)-Width+(Idx rem Width), (Idx rem Width) , -Width)).

gravity_put(_, _, []) -> {error, already_occupied};
gravity_put(Board, NewVal, [Idx|Rest]) -> 
    case put(Board, Idx, NewVal) of
        {ok, Binary} -> 
            {ok, Binary};
        {error, already_occupied} ->
            gravity_put(Board, NewVal, Rest)
    end.

-spec gravity_put_multi(binary(), list(), integer(), integer()) -> {ok,binary()} | {error,atom()}.
gravity_put_multi(Board, _Width, [], _NewVal) -> {ok, Board};
gravity_put_multi(Board, Width, [Idx|Rest], NewVal) ->
    case gravity_put(Board, Width, Idx, NewVal) of
        {error, already_occupied} -> {error, already_occupied};
        {ok, Binary} ->              gravity_put_multi(Binary, Width, Rest, NewVal)
    end.

-spec gravity_put_random(binary(), integer(), integer(), integer()) -> {ok,binary()} | {error,atom()}.
gravity_put_random(Board, _, 0, _) -> {ok, Board};
gravity_put_random(Board, Width, Count, NewVal) ->
    Idx = random_idx0(Width),
    case gravity_put(Board, Width, Idx, NewVal) of 
        {error, already_occupied} -> gravity_put_random(Board, Width, Count, NewVal);
        {ok, Binary} ->              gravity_put_random(Binary, Width, Count-1, NewVal)
    end.

-spec put(binary(), integer(), integer()) -> {ok,binary()} | {error,atom()}.
put(Board, Idx, NewVal) ->
    case binary:at(Board, Idx) of 
        ?AVAILABLE ->
            Prefix = binary:part(Board, 0, Idx),
            Suffix = binary:part(Board, Idx+1, size(Board)-Idx-1),
            {ok, <<Prefix/binary, NewVal:8, Suffix/binary>>};
        _ ->
            {error, already_occupied}
    end.

-spec put_multi(binary(), list(), integer()) -> {ok,binary()} | {error,atom()}.
put_multi(Board, [], _NewVal) -> {ok, Board};
put_multi(Board, [Idx|Rest], NewVal) ->
    case put(Board, Idx, NewVal) of 
        {error, already_occupied} -> {error, already_occupied};
        {ok, Binary} ->              put_multi(Binary, Rest, NewVal)
    end.

-spec put_random(binary(), integer(), integer()) -> {ok,binary()} | {error,atom()}.
put_random(Board, 0, _) -> {ok, Board};
put_random(Board, Count, NewVal) ->
    Idx = random_idx0(size(Board)),
    case put(Board, Idx, NewVal) of 
        {error, already_occupied} -> put_random(Board, Count, NewVal);
        {ok, Binary} ->              put_random(Binary, Count-1, NewVal)
    end.

next_player(Starter,Starter,Other) -> Other;
next_player(Other,Starter,Other) -> Starter.

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
    print_row(1,State).

print_row(N,#state{height=Height}) when N>Height -> ok;
print_row(N, State) -> 
    ?Info("      ~s",[row_str(N,State)]),
    print_row(N+1,State).

row_str(N,#state{width=Width, board=Board}) ->
    [lists:nth(N, ?ROWS),$|] ++ binary_to_list(binary:part(Board, (N-1) * Width, Width)) ++ "|".

-spec is_tie(Board::binary(), Width::integer(), Height::integer(), Run::integer(), Periodic::boolean(), Player::integer(), Next::integer()) -> boolean().
is_tie(Board, Width, Height, Run, Periodic, Player, Next) ->  
    case binary:match(Board, <<?AVAILABLE>>) of
        nomatch -> 
            true;
        _ -> case is_win(binary:replace(Board, <<?AVAILABLE:8>>, <<Player:8>>, [global]), Width, Height, Run, Periodic, Player) of
                true -> 
                    false;
                false ->
                    not is_win(binary:replace(Board, <<?AVAILABLE:8>>, <<Next:8>>, [global]), Width, Height, Run, Periodic, Next)
            end
    end.

is_win(Board, Width, Height, Run, false, Player) -> 
    egambo_tictac_win:win(binary:replace(Board, <<?JOKER:8>>, <<Player:8>>, [global]), Width, Height, Run, Player);
is_win(Board, Width, Height, Run, true, Player) -> 
    egambo_tictac_wip:win(binary:replace(Board, <<?JOKER:8>>, <<Player:8>>, [global]), Width, Height, Run, Player).

play_bot(#state{board=Board, width=Width, height=Height, gravity=Gravity} = State) ->
    Options = put_options(Board, Width, Height, Gravity),
    case play_bot_immediate_win(State, Options) of
        {ok, NewBoard} ->   
            {ok, NewBoard};   % win in this move detected
        _ ->
            case play_bot_defend_immediate(State, Options) of
                {ok, NewBoard} ->   
                    {ok, NewBoard};   % opponent's win in this move detected and taken
                _ ->
                    play_bot_random(State, Options)
            end
    end.

play_bot_random(#state{next=Player, board=Board, gravity=false}, Options) ->
    Idx = lists:nth(random_idx1(length(Options)), Options),
    put(Board, Idx, Player);
play_bot_random(#state{next=Player, board=Board, width=Width, gravity=true}, Options) ->
    gravity_put(Board, Width, lists:nth(random_idx1(length(Options)), Options), Player).

play_bot_immediate_win(_State, []) -> {nok, no_immediate_win};  
play_bot_immediate_win(#state{board=Board, width=Width, height=Height, run=Run, gravity=false, periodic=Periodic, next=Player} = State, [Idx|Rest]) -> 
    {ok, TestBoard} = put(Board, Idx, Player),
    case is_win(TestBoard, Width, Height, Run, Periodic, Player) of
        true ->     {ok, TestBoard};
        false ->    play_bot_immediate_win(State, Rest)
    end;
play_bot_immediate_win(#state{board=Board, width=Width, height=Height, run=Run, gravity=true, periodic=Periodic, next=Player} = State, [Idx|Rest]) -> 
    {ok, TestBoard} = gravity_put(Board, Width, Idx, Player),
    case is_win(TestBoard, Width, Height, Run, Periodic, Player) of
        true ->     {ok, TestBoard};
        false ->    play_bot_immediate_win(State, Rest)
    end.

play_bot_defend_immediate(_State, []) -> {nok, no_immediate_risk};
play_bot_defend_immediate(#state{board=Board, width=Width, height=Height, run=Run, gravity=false, periodic=Periodic, starter=Starter, other=Other, next=Player} = State, [Idx|Rest]) -> 
    Next = next_player(Player, Starter, Other),
    {ok, TestBoard} = put(Board, Idx, Next),
    case is_win(TestBoard, Width, Height, Run, Periodic, Next) of
        true ->     put(Board, Idx, Player);
        false ->    play_bot_defend_immediate(State, Rest)
    end;
play_bot_defend_immediate(#state{board=Board, width=Width, height=Height, run=Run, gravity=true, periodic=Periodic, starter=Starter, other=Other, next=Player} = State, [Idx|Rest]) -> 
    Next = next_player(Player, Starter, Other),
    {ok, TestBoard} = gravity_put(Board, Width, Idx, Next),
    case is_win(TestBoard, Width, Height, Run, Periodic, Next) of
        true ->     gravity_put(Board, Width, Idx, Player);
        false ->    play_bot_defend_immediate(State, Rest)
    end.

put_options(Board, Width, Height, false) ->
    lists:usort([ case B of ?AVAILABLE -> I; _ -> false end || {B,I} <- lists:zip(binary_to_list(Board), lists:seq(0,Width*Height-1))]) -- [false];
put_options(Board, Width, _Height, true) ->
    lists:usort([ case B of ?AVAILABLE -> I; _ -> false end || {B,I} <- lists:zip(binary_to_list(binary:part(Board,0,Width)), lists:seq(0,Width-1))]) -- [false].
