-module(egambo_tictac_sym).

-include("egambo_tictac.hrl").  % import tictac game definitions 

-export([ gen/2         % (W, H) generate symmetry modules
        , norm/5        % (W, H, G, P, Board) ->        {NBoard, Sym}   calculate symmetry operation which normalizes board or output symmetry
        , norm_board/5  % (W, H, G, P, Board) ->        NBoard          calculate normalized board (forget symetry needed to do that)
        , map/4         % (W, H, Board, Sym) ->         NBoard          map board / training input / training output to normalized form
        , map_move/4    % (W, H, Move, Sym) ->          NMove           map 0-based move index back to original move index (add 1 to point to the list item)
        , unmap/4       % (W, H, NBoard, Sym) ->        Board           map normalized form back to original (given norm symmetry)
        , unmap_move/4  % (W, H, NMove, Sym) ->         Move            map 0-based move index back to original move index (add 1 to point to the list item)
        ]).

-define(GRAVITY_SYMMETRIES,[a_ide, b_ver]).
-define(RECT_SYMMETRIES,   [a_ide, b_ver, c_hor, d_pnt]).
-define(SQUARE_SYMMETRIES, [a_ide, b_ver, c_hor, d_pnt, e_bck, f_fwd, g_lft, h_rgt]).

-define(SYM_MODULE(__W, __H), list_to_atom(atom_to_list(?MODULE) ++ lists:flatten(io_lib:format("_~p_~p",[__W, __H])))).

-safe(all).

sym_export(_W, _H, Symmetries) ->
    F = fun(X) -> atom_to_list(X) end,
    lists:flatten(lists:join("/1, ", lists:map(F, Symmetries)) ++ "/1").

tx_sym(TX) -> list_to_atom([$x, $0 + TX div 10, $0 + TX rem 10]).

ty_sym(TY) -> list_to_atom([$y, $0 + TY div 10, $0 + TY rem 10]).

tx_symmetries(W) -> [tx_sym(TX) || TX <- lists:seq(1, W-1)].

ty_symmetries(H) -> [ty_sym(TY) || TY <- lists:seq(1, H-1)].

tx_export(W) ->
    F = fun(X) -> atom_to_list(X) end,
    lists:flatten(lists:join("/1, ", lists:map(F, tx_symmetries(W))) ++ "/1").

ty_export(W) ->
    F = fun(X) -> atom_to_list(X) end,
    lists:flatten(lists:join("/1, ", lists:map(F, ty_symmetries(W))) ++ "/1").

gen_render(_, 1, a_ide) -> <<"a_ide(Board) -> Board.\n\n">>;     % Identity transformation
gen_render(_, _, a_ide) -> <<>>;                  % Can use identity transformation from gravity
gen_render(W, 1, S) ->
    Symmetry = atom_to_binary(S, utf8), 
    InPat = gen_input_pattern(W, 1, 0, 0, []),
    OutPat = gen_output_pattern(W, 1, S, 0, 0, []),
    <<Symmetry/binary, "([", InPat/binary, "]) -> \n    [", OutPat/binary, "];\n">>;
gen_render(W, H, S) ->
    Symmetry = atom_to_binary(S, utf8), 
    InPat = gen_input_pattern(W, H, 0, 0, []),
    OutPat = gen_output_pattern(W, H, S, 0, 0, []),
    <<Symmetry/binary, "([", InPat/binary, "]) -> \n    [", OutPat/binary, "].\n\n">>.

var_name(X,Y) -> [$A+Y, $A+X].

gen_input_pattern(_, H, 0, H, Acc) -> list_to_binary(lists:nthtail(2, lists:flatten(lists:reverse(Acc))));
gen_input_pattern(W, H, W, Y, Acc) -> gen_input_pattern(W, H, 0, Y+1, Acc); 
gen_input_pattern(W, H, X, Y, Acc) -> gen_input_pattern(W, H, X+1, Y, [", " ++ var_name(X,Y)|Acc]).

gen_output_pattern(_, H, _, 0, H, Acc) -> list_to_binary(lists:nthtail(2, lists:flatten(lists:reverse(Acc))));
gen_output_pattern(W, H, S, W, Y, Acc) -> gen_output_pattern(W, H, S, 0, Y+1, Acc); 
gen_output_pattern(W, H, b_ver=S, X, Y, Acc) -> gen_output_pattern(W, H, S, X+1, Y, [", " ++ var_name(W-1-X,Y)|Acc]);
gen_output_pattern(W, H, c_hor=S, X, Y, Acc) -> gen_output_pattern(W, H, S, X+1, Y, [", " ++ var_name(X,H-1-Y)|Acc]);
gen_output_pattern(W, H, d_pnt=S, X, Y, Acc) -> gen_output_pattern(W, H, S, X+1, Y, [", " ++ var_name(W-1-X,H-1-Y)|Acc]);
gen_output_pattern(W, H, e_bck=S, X, Y, Acc) -> gen_output_pattern(W, H, S, X+1, Y, [", " ++ var_name(Y,X)|Acc]);
gen_output_pattern(W, H, f_fwd=S, X, Y, Acc) -> gen_output_pattern(W, H, S, X+1, Y, [", " ++ var_name(H-1-Y,W-1-X)|Acc]);
gen_output_pattern(W, H, g_lft=S, X, Y, Acc) -> gen_output_pattern(W, H, S, X+1, Y, [", " ++ var_name(W-1-Y,X)|Acc]);
gen_output_pattern(W, H, h_rgt=S, X, Y, Acc) -> gen_output_pattern(W, H, S, X+1, Y, [", " ++ var_name(Y,W-1-X)|Acc]).

gen_render_tx(W, 1, S) ->
    Symmetry = atom_to_binary(S, utf8), 
    InPat = gen_input_pattern(W, 1, 0, 0, []),
    OutPat = gen_output_pattern_tx(W, 1, S, 0, 0, []),
    binary_to_list(<<Symmetry/binary, "([", InPat/binary, "]) -> \n    [", OutPat/binary, "];\n">>);
gen_render_tx(W, H, S) -> 
    Symmetry = atom_to_binary(S, utf8), 
    InPat = gen_input_pattern(W, H, 0, 0, []),
    OutPat = gen_output_pattern_tx(W, H, S, 0, 0, []),
    binary_to_list(<<Symmetry/binary, "([", InPat/binary, "]) -> \n    [", OutPat/binary, "].\n\n">>).

tx_from_sym(W, X, S) -> 
    [$x,N1,N0] = atom_to_list(S),
    (X + W - 10*(N1-$0) - N0+$0) rem W.

gen_output_pattern_tx(_, H, _, 0, H, Acc) -> list_to_binary(lists:nthtail(2, lists:flatten(lists:reverse(Acc))));
gen_output_pattern_tx(W, H, S, W, Y, Acc) -> gen_output_pattern_tx(W, H, S, 0, Y+1, Acc); 
gen_output_pattern_tx(W, H, S, X, Y, Acc) -> gen_output_pattern_tx(W, H, S, X+1, Y, [", " ++ var_name(tx_from_sym(W, X, S), Y)|Acc]).

gen_render_ty(W, H, S) -> 
    Symmetry = atom_to_binary(S, utf8), 
    InPat = gen_input_pattern(W, H, 0, 0, []),
    OutPat = gen_output_pattern_ty(W, H, S, 0, 0, []),
    binary_to_list(<<Symmetry/binary, "([", InPat/binary, "]) -> \n    [", OutPat/binary, "].\n\n">>).

ty_from_sym(H, Y, S) -> 
    [$y,N1,N0] = atom_to_list(S),
    (Y + H - 10*(N1-$0) - N0+$0) rem H.

gen_output_pattern_ty(_, H, _, 0, H, Acc) -> list_to_binary(lists:nthtail(2, lists:flatten(lists:reverse(Acc))));
gen_output_pattern_ty(W, H, S, W, Y, Acc) -> gen_output_pattern_ty(W, H, S, 0, Y+1, Acc); 
gen_output_pattern_ty(W, H, S, X, Y, Acc) -> gen_output_pattern_ty(W, H, S, X+1, Y, [", " ++ var_name(X, ty_from_sym(H, Y, S))|Acc]).

gen(W, W) ->     % quadratic board
    file:write_file("src/" ++ atom_to_list(?SYM_MODULE(W, W)) ++ ".erl", 
      io_lib:format("-module(~s).~n~n-export([~s]).~n~n-export([~s]).~n~n-export([~s]).~n~n% generated in ~s~n~n~s~n~s~n~s~n~s~n",
        [ ?SYM_MODULE(W, W)
        , sym_export(W, W, ?SQUARE_SYMMETRIES)
        , tx_export(W)
        , ty_export(W)
        , ?MODULE
        , list_to_binary([gen_render(W, 1, S) || S <- ?GRAVITY_SYMMETRIES])
        , list_to_binary([gen_render(W, W, S) || S <- ?SQUARE_SYMMETRIES])
        , list_to_binary([gen_render_tx(W, 1, S) ++ gen_render_tx(W, W, S) || S <- tx_symmetries(W)])
        , list_to_binary([gen_render_ty(W, W, S) || S <- ty_symmetries(W)])
        ]));
gen(W, H) ->
    file:write_file("src/" ++ atom_to_list(?SYM_MODULE(W, H)) ++ ".erl", 
      io_lib:format("-module(~s).~n~n-export([~s]).~n~n-export([~s]).~n~n-export([~s]).~n~n% generated in ~s~n~n~s~n~s~n~s~n~s~n",
        [ ?SYM_MODULE(W, H)
        , sym_export(W, W, ?RECT_SYMMETRIES)
        , tx_export(W)
        , ty_export(H)
        , ?MODULE
        , list_to_binary([gen_render(W, 1, S) || S <- ?GRAVITY_SYMMETRIES])
        , list_to_binary([gen_render(W, H, S) || S <- ?RECT_SYMMETRIES])
        , list_to_binary([gen_render_tx(W, 1, S) ++ gen_render_tx(W, H, S) || S <- tx_symmetries(W)])
        , list_to_binary([gen_render_ty(W, H, S) || S <- ty_symmetries(H)])
        ])).

transx(W, _H, TX, Board) when TX==0;TX==W -> Board;  
transx(W, H, TX, Board) -> apply(?SYM_MODULE(W, H), tx_sym(TX), [Board]). 

transy(_W, H, TY, Board) when TY==0;TY==H -> Board;
transy(W, H, TY, Board) -> apply(?SYM_MODULE(W, H), ty_sym(TY), [Board]).

map(W, H, Board, Sym) when is_atom(Sym) -> 
    apply(?SYM_MODULE(W, H), Sym, [Board]);
map(W, H, Board, {S,TX,TY}) when is_atom(S) -> 
    transy(W, H, TY, transx(W, H, TX, apply(?SYM_MODULE(W, H), S, [Board]))). 

unmap(_, _, NBoard, a_ide) -> NBoard;
unmap(W, H, NBoard, S) when S==b_ver;S==c_hor;S==d_pnt;S==e_bck;S==f_fwd -> map(W, H, NBoard, S);
unmap(W, H, NBoard, {S,0,0}) -> unmap(W, H, NBoard, S);
unmap(W, H, NBoard, g_lft) -> map(W, H, NBoard, h_rgt);
unmap(W, H, NBoard, h_rgt) -> map(W, H, NBoard, g_lft);
unmap(W, H, NBoard, {S,TX,TY}) -> unmap(W, H, transx(W, H, W-TX, transy(W, H, H-TY, NBoard)), S).

t_off(Dim) -> lists:seq(0,Dim-1).

pick_norm(L) -> hd(lists:sort(L)).  % simple sorting of board -> pushes action to high X and Y coordinates

-spec norm(W::integer(), H::integer(), Gravity::boolean(), Periodic::boolean(), Board::list()) -> {NewBoard::list(), Symmetry::tuple()}.
norm(W, H, true, false, Board) ->
    pick_norm([{map(W, H, Board, S), S} || S <- ?GRAVITY_SYMMETRIES]);
norm(W, H, true, true, Board) ->
    pick_norm([{map(W, H, Board, {S,TX,0}), {S,TX,0}} || S <- ?GRAVITY_SYMMETRIES, TX <- t_off(W)]);
norm(W, W, false, false, Board) ->
    pick_norm([{map(W, W, Board, S), S} || S <- ?SQUARE_SYMMETRIES]);
norm(W, W, false, true, Board) ->
    pick_norm([{map(W, W, Board, {S,TX,TY}), {S,TX,TY}} || S <- ?SQUARE_SYMMETRIES, TX <- t_off(W), TY <- t_off(W)]);
norm(W, H, false, false, Board) ->
    pick_norm([{map(W, H, Board, S), S} || S <- ?RECT_SYMMETRIES]);
norm(W, H, false, true, Board) ->
    pick_norm([{map(W, H, Board, {S,TX,TY}), {S,TX,TY}} || S <- ?RECT_SYMMETRIES, TX <- t_off(W), TY <- t_off(H)]).

norm_board(W, H, Gravity, Periodic, Board) -> element(1, norm(W, H, Gravity, Periodic, Board)).

x(W, _H, Idx) -> Idx rem W.

y(W, _H, Idx) -> Idx div W.

idx(W, H, X, Y) -> (X+W) rem W + W*((Y+H) rem H). 

map_move(W, H, Idx, S) when is_atom(S) ->    
    map_sym(W, H, Idx, S);
map_move(W, H, Idx, {S,0,0}) ->    
    map_sym(W, H, Idx, S);
map_move(W, H, Idx, {S,TX,TY}) -> 
    IdxS = map_sym(W, H, Idx, S),
    IdxX = idx(W, H, (x(W, H, IdxS)+TX) rem W, y(W, H, IdxS)),
    idx(W, H, x(W, H, IdxX), (y(W, H, IdxX)+TY) rem H).

map_sym(_, _, Idx, a_ide) ->    Idx;
map_sym(W, H, Idx, b_ver) ->    idx(W, H, W-1-x(W, H, Idx), y(W, H, Idx));
map_sym(W, H, Idx, c_hor) ->    idx(W, H, x(W, H, Idx), H-1-y(W, H, Idx));
map_sym(W, H, Idx, d_pnt) ->    idx(W, H, W-1-x(W, H, Idx), H-1-y(W, H, Idx));
map_sym(W, H, Idx, e_bck) ->    idx(W, H, y(W, H, Idx), x(W, H, Idx));
map_sym(W, H, Idx, f_fwd) ->    idx(W, H, H-1-y(W, H, Idx), W-1-x(W, H, Idx));
map_sym(W, H, Idx, g_lft) ->    idx(W, H, y(W, H, Idx), W-1-x(W, H, Idx));
map_sym(W, H, Idx, h_rgt) ->    idx(W, H, W-1-y(W, H, Idx), x(W, H, Idx)).

unmap_move(W, H, Idx, S) when is_atom(S) ->    
    unmap_sym(W, H, Idx, S);
unmap_move(W, H, Idx, {S,0,0}) ->    
    unmap_sym(W, H, Idx, S);
unmap_move(W, H, Idx, {S,TX,TY}) -> 
    IdxY = idx(W, H, x(W, H, Idx), (y(W, H, Idx)+H-TY) rem H),
    IdxX = idx(W, H, (x(W, H, IdxY)+W-TX) rem W, y(W, H, IdxY)),
    unmap_sym(W, H, IdxX, S).

unmap_sym(W, H, Idx, h_rgt) ->  map_sym(W, H, Idx, g_lft);
unmap_sym(W, H, Idx, g_lft) ->  map_sym(W, H, Idx, h_rgt);
unmap_sym(W, H, Idx, Sym) ->  map_sym(W, H, Idx, Sym).

%% ===================================================================
%% TESTS
%% ===================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

map_33_test_() ->
    W = 3,
    H = 3,
    Board = lists:seq($a,$a+W*H-1),
    [ {"a_ide",   ?_assertEqual("abcdefghi", map(W, H, Board, a_ide))}
    , {"b_ver",   ?_assertEqual("cbafedihg", map(W, H, Board, b_ver))}
    , {"c_hor",   ?_assertEqual("ghidefabc", map(W, H, Board, c_hor))}
    , {"d_pnt",   ?_assertEqual("ihgfedcba", map(W, H, Board, d_pnt))}
    , {"e_bck",   ?_assertEqual("adgbehcfi", map(W, H, Board, e_bck))}
    , {"f_fwd",   ?_assertEqual("ifchebgda", map(W, H, Board, f_fwd))}
    , {"h_rgt",   ?_assertEqual("gdahebifc", map(W, H, Board, h_rgt))}
    , {"g_lft",   ?_assertEqual("cfibehadg", map(W, H, Board, g_lft))}
    , {"a_idex1", ?_assertEqual("cabfdeigh", map(W, H, Board, {a_ide,1,0}))}
    , {"a_idex2", ?_assertEqual("bcaefdhig", map(W, H, Board, {a_ide,2,0}))}
    , {"a_idey1", ?_assertEqual("ghiabcdef", map(W, H, Board, {a_ide,0,1}))}
    , {"a_idey2", ?_assertEqual("defghiabc", map(W, H, Board, {a_ide,0,2}))}
    , {"f_fwdx2", ?_assertEqual("fciebhdag", map(W, H, Board, {f_fwd,2,0}))}
    , {"f_fwdy2", ?_assertEqual("hebgdaifc", map(W, H, Board, {f_fwd,0,2}))}
    , {"rgtx1y1", ?_assertEqual("cifagdbhe", map(W, H, Board, {h_rgt,1,1}))}
    , {"rgtx1y2", ?_assertEqual("bhecifagd", map(W, H, Board, {h_rgt,1,2}))}
    ].

map_move_33_test_() ->
    W = 3,
    H = 3,
    % Board = lists:seq($a,$a+W*H-1),
    [ {"f_fwdx2y0", ?_assertEqual(7, map_move(W, H, 0, {f_fwd,2,0}))}
    , {"f_fwdx0y2", ?_assertEqual(5, map_move(W, H, 0, {f_fwd,0,2}))}
    , {"h_rgtx1y1", ?_assertEqual(3, map_move(W, H, 0, {h_rgt,1,1}))}
    , {"h_rgtx1y2", ?_assertEqual(6, map_move(W, H, 0, {h_rgt,1,2}))}
    ].

map_44_test_() ->
    W = 4,
    H = 4,
    Board = lists:seq($a,$a+W*H-1),
    [ {"a_ide",   ?_assertEqual("abcdefghijklmnop", map(W, H, Board, a_ide))}
    , {"b_ver",   ?_assertEqual("dcbahgfelkjiponm", map(W, H, Board, b_ver))}
    , {"c_hor",   ?_assertEqual("mnopijklefghabcd", map(W, H, Board, c_hor))}
    , {"d_pnt",   ?_assertEqual("ponmlkjihgfedcba", map(W, H, Board, d_pnt))}
    , {"e_bck",   ?_assertEqual("aeimbfjncgkodhlp", map(W, H, Board, e_bck))}
    , {"f_fwd",   ?_assertEqual("plhdokgcnjfbmiea", map(W, H, Board, f_fwd))}
    , {"g_lft",   ?_assertEqual("dhlpcgkobfjnaeim", map(W, H, Board, g_lft))}
    , {"h_rgt",   ?_assertEqual("mieanjfbokgcplhd", map(W, H, Board, h_rgt))}
    , {"a_idex1", ?_assertEqual("dabchefglijkpmno", map(W, H, Board, {a_ide,1,0}))}
    , {"rgtx1y1", ?_assertEqual("dplhamiebnjfcokg", map(W, H, Board, {h_rgt,1,1}))}
    ].

sym_44_test_() ->
    W = 4,
    H = 4,
    Board = egambo_tictac:shuffle(lists:seq($A,$A+W*H-1)),
    [ {"a_ide", ?_assertEqual(Board, map(W, H, map(W, H, Board, a_ide), a_ide))}
    , {"b_ver", ?_assertEqual(Board, map(W, H, map(W, H, Board, b_ver), b_ver))}
    , {"c_hor", ?_assertEqual(Board, map(W, H, map(W, H, Board, c_hor), c_hor))}
    , {"d_pnt", ?_assertEqual(Board, map(W, H, map(W, H, Board, d_pnt), d_pnt))}
    , {"e_bck", ?_assertEqual(Board, map(W, H, map(W, H, Board, e_bck), e_bck))}
    , {"f_fwd", ?_assertEqual(Board, map(W, H, map(W, H, Board, f_fwd), f_fwd))}
    , {"g_lft", ?_assertEqual(Board, map(W, H, map(W, H, Board, g_lft), h_rgt))}
    , {"h_rgt", ?_assertEqual(Board, map(W, H, map(W, H, Board, h_rgt), g_lft))}
    ].

sym_44g_test_() ->
    W = 4,
    H = 4,
    Board = egambo_tictac:shuffle(lists:seq($A,$A+W-1)),
    [ {"a_ide", ?_assertEqual(Board, map(W, H, map(W, H, Board, a_ide), a_ide))}
    , {"b_ver", ?_assertEqual(Board, map(W, H, map(W, H, Board, b_ver), b_ver))}
    ].
   
sym_76_test_() ->
    W = 7,
    H = 6,
    Board = egambo_tictac:shuffle(lists:seq($a,$a+W*H-1)),
    [ {"a_ide", ?_assertEqual(Board, map(W, H, map(W, H, Board, a_ide), a_ide))}
    , {"b_ver", ?_assertEqual(Board, map(W, H, map(W, H, Board, b_ver), b_ver))}
    , {"c_hor", ?_assertEqual(Board, map(W, H, map(W, H, Board, c_hor), c_hor))}
    , {"d_pnt", ?_assertEqual(Board, map(W, H, map(W, H, Board, d_pnt), d_pnt))}
    ].

sym_76g_test_() ->
    W = 7,
    H = 6,
    Board = egambo_tictac:shuffle(lists:seq($a,$a+W-1)),
    [ {"a_ide", ?_assertEqual(Board, map(W, H, map(W, H, Board, a_ide), a_ide))}
    , {"b_ver", ?_assertEqual(Board, map(W, H, map(W, H, Board, b_ver), b_ver))}
    ].

label(Term) -> lists:flatten(io_lib:format("~p",[Term])).

norm_test(W, H, G, P, Board) ->     
    % will norm followed by unmap return the original board ?
    {NBoard, S} = norm(W, H, G, P, Board),
    {label({Board,S}), ?_assertEqual(Board, unmap(W, H, NBoard, S))}.

norm_tup(W, H, G, P) -> norm_test(W, H, G, P, egambo_tictac:shuffle(lists:seq($A,$A+W*H-1))).

norm_44_test_() -> [norm_tup(4,4,false,false) || _ <- lists:seq(0,19)].

norm_44p_test_() -> [norm_tup(4,4,false,true) || _ <- lists:seq(0,19)].

norm_54_test_() -> [norm_tup(5,4,false,false) || _ <- lists:seq(0,5)].

norm_54p_test_() -> [norm_tup(5,4,false,true) || _ <- lists:seq(0,5)].

norm_76g_test_() -> [norm_tup(7,6,true,false) || _ <- lists:seq(0,5)].

norm_76gp_test_() -> [norm_tup(7,6,true,true) || _ <- lists:seq(0,5)].

map_move_test(W, H, G, P, Board) -> 
    % can we get back the original board by looking up elements in the normalized board?
    {NBoard, S} = norm(W, H, G, P, Board),
    {label({Board,S}), ?_assertEqual(Board, [lists:nth(map_move(W, H, Idx, S)+1, NBoard) || Idx <- lists:seq(0, W*H-1)])}.

map_move_tup(W, H, G, P) -> map_move_test(W, H, G, P, egambo_tictac:shuffle(lists:seq($A,$A+W*H-1))).

map_move_33p_test_() -> [map_move_tup(3, 3, false, true) || _ <- lists:seq(0,9)].

map_move_44_test_() -> [map_move_tup(4, 4, false, false) || _ <- lists:seq(0,9)].

map_move_44p_test_() -> [map_move_tup(4, 4, false, true) || _ <- lists:seq(0,9)].

map_move_54_test_() -> [map_move_tup(5, 4, false, false) || _ <- lists:seq(0,5)].

map_move_54p_test_() -> [map_move_tup(5, 4, false, true) || _ <- lists:seq(0,5)].

map_move_76g_test_() -> [map_move_tup(7, 6, true, false) || _ <- lists:seq(0,5)].

map_move_76gp_test_() -> [map_move_tup(7, 6, true, true) || _ <- lists:seq(0,5)].

unmap_move_test(W, H, G, P, Board) -> 
    % can we get the normalized board by looking up elements in the original board?
    {NBoard, S} = norm(W, H, G, P, Board),
    {label({Board,S}), ?_assertEqual(NBoard, [lists:nth(unmap_move(W, H, Idx, S)+1, Board) || Idx <- lists:seq(0, W*H-1)])}.

unmap_move_tup(W, H, G, P) -> unmap_move_test(W, H, G, P, egambo_tictac:shuffle(lists:seq($A,$A+W*H-1))).

unmap_move_44_test_() -> [unmap_move_tup(4, 4, false, false) || _ <- lists:seq(0,19)].

unmap_move_54_test_() -> [unmap_move_tup(5, 4, false, false) || _ <- lists:seq(0,5)].

unmap_move_76g_test_() -> [unmap_move_tup(7, 6, true, false) || _ <- lists:seq(0,5)].

-endif.
