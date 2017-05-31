-module(egambo_tictac_wip).

-export([ gen/3     % generate periodic wins
        ]).

idx(W, H, X, Y) -> (X+W) rem W + W*((Y+H) rem H). 

gen_skip(Pos) -> list_to_binary([$_,$:] ++ integer_to_list(8*Pos) ++ [$,,32]).

gen_render(W,H,R,L) -> 
    Pat = gen_pattern(W,H,R,-1,L,[]),
    <<"win(<<", Pat/binary, "_/binary>>, P) -> true;\n">>.

gen_render_else(_W, _H, _R) ->
    <<"win(_, _) -> false.\n">>.

gen_pattern(_, _, _, _, [], Acc) -> list_to_binary(lists:reverse(Acc));
gen_pattern(W, H, R, Last, [Next|Rest], Acc) when Next==Last+1 -> 
    gen_pattern(W, H, R, Next, Rest, [<<"P:8, ">>|Acc]);     % no skip
gen_pattern(W, H, R, Last, [Next|Rest], Acc) -> 
    gen_pattern(W, H, R, Next, Rest, [<<"P:8, ">>, gen_skip(Next-Last-1) |Acc]).

gen(W, H, R) -> 
    file:write_file("src/" ++ atom_to_list(egambo_tictac:win_module(W, H, R, true)) ++ ".erl", 
      io_lib:format("-module(~s).~n~n-export([win/2]).~n~n% generated in ~s~n~n~s~s~n",
        [ egambo_tictac:win_module(W, H, R, true)
        , ?MODULE
        , list_to_binary(
          [ gen_render(W,H,R,L) 
            || 
            L <-  lists:usort(gen_hor(W, H, R) ++ gen_ver(W, H, R) ++ gen_bck(W, H, R) ++ gen_fwd(W, H, R))
          ]
        )
        , gen_render_else(W, H, R)
        ])).

gen_hor(W, H, R) -> 
    lists:usort([g_hor(W, H, R, X, Y) || Y <- lists:seq(0,H-1), X <- lists:seq(0,W-1)]). 

g_hor(W, H, R, X, Y) ->
    lists:usort([idx(W, H, X+O, Y) || O <- lists:seq(0,R-1)]).

gen_ver(W, H, R) ->  
    lists:usort([g_ver(W, H, R, X, Y) || Y <- lists:seq(0,H-1), X <- lists:seq(0,W-1)]). 

g_ver(W, H, R, X, Y) ->
    lists:usort([idx(W, H, X, Y+O) || O <- lists:seq(0,R-1)]).

gen_bck(W, H, R) -> 
    lists:usort([g_bck(W, H, R, X, Y) || Y <- lists:seq(0,H-1), X <- lists:seq(0,W-1)]). 

g_bck(W, H, R, X, Y) ->
    lists:usort([idx(W, H, X-O, Y+O) || O <- lists:seq(0,R-1)]).

gen_fwd(W, H, R) -> 
    lists:usort([g_fwd(W, H, R, X, Y) || Y <- lists:seq(0,H-1), X <- lists:seq(0,W-1)]). 

g_fwd(W, H, R, X, Y) ->
    lists:usort([idx(W, H, X+O, Y+O) || O <- lists:seq(0,R-1)]).

