-module(egambo_tictac_sym).

-export([ gen/2         % (W, H) generate symmetry modules
        , norm/5        % (W, H, G, P, Board) ->        {NBoard, Sym}
        , norm_board/5  % (W, H, G, P, Board) ->        NBoard
%        , norm_move/6   % (W, H, Move, Sym) ->          NMove
        , denorm/4      % (W, H, NBoard, Sym) ->        Board
        , denorm_move/4 % (W, H, Sym, NMove) ->         Move
        , test/2
        ]).

-define(GRAVITY_SYMMETRIES,[ide, ver]).
-define(RECT_SYMMETRIES,   [ide, ver, hor, pnt]).
-define(SQUARE_SYMMETRIES, [ide, ver, hor, pnt, bck, fwd, lft, rgt]).

-define(SYM_MODULE(__W, __H), list_to_atom(atom_to_list(?MODULE) ++ lists:flatten(io_lib:format("_~p_~p",[__W, __H])))).

sym_export(_W, _H, Symmetries) ->
    F = fun(X) -> atom_to_list(X) end,
    lists:flatten(lists:join("/1, ", lists:map(F, Symmetries)) ++ "/1").

gen_render(_, _, ide) ->
    <<"ide(Board) -> Board.\n\n">>;     % Identity transformation
gen_render(W, H, S) ->
    Symmetry = atom_to_binary(S, utf8), 
    InPat = gen_input_pattern(W, H, 0, 0, []),
    OutPat = gen_output_pattern(W, H, S, 0, 0, []),
    <<Symmetry/binary, "(<<", InPat/binary, ">>) -> \n    <<", OutPat/binary, ">>.\n\n">>.

var_name(X,Y) -> [$A+Y, $A+X].

gen_input_pattern(_, H, 0, H, Acc) -> list_to_binary(lists:nthtail(2, lists:flatten(lists:reverse(Acc))));
gen_input_pattern(W, H, W, Y, Acc) -> gen_input_pattern(W, H, 0, Y+1, Acc); 
gen_input_pattern(W, H, X, Y, Acc) -> gen_input_pattern(W, H, X+1, Y, [", " ++ var_name(X,Y) ++ ":8"|Acc]).

gen_output_pattern(_, H, _, 0, H, Acc) -> list_to_binary(lists:nthtail(2, lists:flatten(lists:reverse(Acc))));
gen_output_pattern(W, H, S, W, Y, Acc) -> gen_output_pattern(W, H, S, 0, Y+1, Acc); 
gen_output_pattern(W, H, ver=S, X, Y, Acc) -> gen_output_pattern(W, H, S, X+1, Y, [", " ++ var_name(W-1-X,Y) ++ ":8"|Acc]);
gen_output_pattern(W, H, hor=S, X, Y, Acc) -> gen_output_pattern(W, H, S, X+1, Y, [", " ++ var_name(X,H-1-Y) ++ ":8"|Acc]);
gen_output_pattern(W, H, pnt=S, X, Y, Acc) -> gen_output_pattern(W, H, S, X+1, Y, [", " ++ var_name(W-1-X,H-1-Y) ++ ":8"|Acc]);
gen_output_pattern(W, H, bck=S, X, Y, Acc) -> gen_output_pattern(W, H, S, X+1, Y, [", " ++ var_name(Y,X) ++ ":8"|Acc]);
gen_output_pattern(W, H, fwd=S, X, Y, Acc) -> gen_output_pattern(W, H, S, X+1, Y, [", " ++ var_name(H-1-Y,W-1-X) ++ ":8"|Acc]);
gen_output_pattern(W, H, lft=S, X, Y, Acc) -> gen_output_pattern(W, H, S, X+1, Y, [", " ++ var_name(W-1-Y,X) ++ ":8"|Acc]);
gen_output_pattern(W, H, rgt=S, X, Y, Acc) -> gen_output_pattern(W, H, S, X+1, Y, [", " ++ var_name(Y,W-1-X) ++ ":8"|Acc]).

gen(W, W) ->     % quadratic board
    file:write_file("src/" ++ atom_to_list(?SYM_MODULE(W, W)) ++ ".erl", 
      io_lib:format("-module(~s).~n~n-export([~s]).~n~n% generated in ~s~n~n~s~n",
        [ ?SYM_MODULE(W, W)
        , sym_export(W, W, ?SQUARE_SYMMETRIES)
        , ?MODULE
        , list_to_binary([gen_render(W, W, S) || S <- ?SQUARE_SYMMETRIES])
        ]));
gen(W, H) ->
    file:write_file("src/" ++ atom_to_list(?SYM_MODULE(W, H)) ++ ".erl", 
      io_lib:format("-module(~s).~n~n-export([~s]).~n~n% generated in ~s~n~n~s~n",
        [ ?SYM_MODULE(W, H)
        , sym_export(W, W, ?RECT_SYMMETRIES)
        , ?MODULE
        , list_to_binary([gen_render(W, H, S) || S <- ?RECT_SYMMETRIES])
        ])).

-spec norm(W::integer(), H::integer(), Gravity::boolean(), Periodic::boolean(), Board::binary()) -> {NewBoard::binary(), Symmetry::tuple()}.
norm(W, H, true, false, Board) ->
    pick_norm([{apply(?SYM_MODULE(W, H), S, [Board]), {S,0,0}} || S <- ?GRAVITY_SYMMETRIES]);
norm(W, H, true, true, Board) ->
    pick_norm([{apply(?SYM_MODULE(W, H), S, [trans(Board,W,H,TX,0)]), {S,TX,0}} || S <- ?GRAVITY_SYMMETRIES, TX <- t_off(W)]);
norm(W, W, false, false, Board) ->
    pick_norm([{apply(?SYM_MODULE(W, W), S, [Board]), {S,0,0}} || S <- ?SQUARE_SYMMETRIES]);
norm(W, W, false, true, Board) ->
    pick_norm([{apply(?SYM_MODULE(W, W), S, [trans(Board,W,W,TX,TY)]), {S,TX,TY}} || S <- ?SQUARE_SYMMETRIES, TX <- t_off(W), TY <- t_off(W)]);
norm(W, H, false, false, Board) ->
    pick_norm([{apply(?SYM_MODULE(W, H), S, [Board]), {S,0,0}} || S <- ?RECT_SYMMETRIES]);
norm(W, H, false, true, Board) ->
    pick_norm([{apply(?SYM_MODULE(W, H), S, [trans(Board,W,H,TX,TY)]), {S,TX,TY}} || S <- ?RECT_SYMMETRIES, TX <- t_off(W), TY <- t_off(H)]).

norm_board(W, H, Gravity, Periodic, Board) -> element(1, norm(W, H, Gravity, Periodic, Board)).

pick_norm(L) -> element(1, lists:sort(L)).  % simple sorting of board -> pushes action to high X and Y coordinates

t_off(Dim) -> lists:seq(0,Dim-1).

trans(Board, W, H, TX, TY) -> transy(W, H, transx(Board, W, H, TX), TY).

transx(Board, W, _H, TX) when TX==0;TX==W -> Board;  
transx(Board, _W, _H, _TX) -> Board.     %ToDo: implement X shifting 

transy(Board, _W, H, TY) when TY==0;TY==H -> Board;
transy(Board, _W, _H, _TY) -> Board.     %ToDo: implement Y shifting

denorm(W, H, NBoard, {S,0,0}) when S==hor;S==ver;S==pnt;S==bck;S==fwd -> apply(?SYM_MODULE(W, H), S, [NBoard]);
denorm(W, H, NBoard, {lft,0,0}) -> apply(?SYM_MODULE(W, H), rgt, [NBoard]);
denorm(W, H, NBoard, {rgt,0,0}) -> apply(?SYM_MODULE(W, H), lft, [NBoard]);
denorm(W, H, NBoard, {S,TX,TY}) -> transx(transy(denorm(W, H, NBoard, {S,0,0}), W, H, H-TY), W, H, W-TX).


x(W, H, Idx) -> Idx rem W.

y(W, H, Idx) -> Idx div W.

idx(W, H, X, Y) -> (X+W) rem W + W*((Y+H) rem H). 

denorm_move(W, H, {S,0,0}, Idx) ->    
    denorm_s(W, H, S, Idx);
denorm_move(W, H, {S,TX,TY}, Idx) -> 
    IdxS = denorm_s(W, H, S, Idx),
    IdxY = idx(W, H, x(W, H, IdxS), y(W, H, IdxS)+H-TY),
    idx(W, H, x(W, H, IdxY)+W-TX, y(W, H, IdxY)).

denorm_s(_, _, ide, Idx) ->    Idx;
denorm_s(W, H, ver, Idx) ->    idx(W, H, W-1-x(W, H, Idx), y(W, H, Idx));
denorm_s(W, H, hor, Idx) ->    idx(W, H, x(W, H, Idx), H-1-y(W, H, Idx));
denorm_s(W, H, pnt, Idx) ->    idx(W, H, W-1-x(W, H, Idx), H-1-y(W, H, Idx));
denorm_s(W, H, bck, Idx) ->    idx(W, H, y(W, H, Idx), x(W, H, Idx));
denorm_s(W, H, fwd, Idx) ->    idx(W, H, H-1-y(W, H, Idx), W-1-x(W, H, Idx));
denorm_s(W, H, lft, Idx) ->    idx(W, H, y(W, H, Idx), W-1-x(W, H, Idx));
denorm_s(W, H, rgt, Idx) ->    idx(W, H, W-1-y(W, H, Idx), x(W, H, Idx)).
   
test(W, H) ->
    Mod = ?SYM_MODULE(W, H),
    Board = list_to_binary(lists:seq($a,$a+W*H-1)),
    Board = apply(Mod, ide, [apply(Mod, ide, [Board])]),
    Board = apply(Mod, ver, [apply(Mod, ver, [Board])]),
    Board = apply(Mod, hor, [apply(Mod, hor, [Board])]),
    Board = apply(Mod, pnt, [apply(Mod, pnt, [Board])]),
    case H of
        W ->
            Board = apply(Mod, bck, [apply(Mod, bck, [Board])]),
            Board = apply(Mod, fwd, [apply(Mod, fwd, [Board])]),
            Board = apply(Mod, lft, [apply(Mod, rgt, [Board])]),
            Board = apply(Mod, rgt, [apply(Mod, lft, [Board])]),
            ok;
        _ -> 
            ok
    end.