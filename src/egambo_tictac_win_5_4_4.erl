-module(egambo_tictac_win_5_4_4).

-export([win/2]).

% generated in egambo_tictac_win

win(<<P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<P:8, _:32, P:8, _:32, P:8, _:32, P:8, _/binary>>, P) -> true;
win(<<P:8, _:40, P:8, _:40, P:8, _:40, P:8, _/binary>>, P) -> true;
win(<<_:8, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:8, P:8, _:32, P:8, _:32, P:8, _:32, P:8, _/binary>>, P) -> true;
win(<<_:8, P:8, _:40, P:8, _:40, P:8, _:40, P:8, _/binary>>, P) -> true;
win(<<_:16, P:8, _:32, P:8, _:32, P:8, _:32, P:8, _/binary>>, P) -> true;
win(<<_:24, P:8, _:24, P:8, _:24, P:8, _:24, P:8, _/binary>>, P) -> true;
win(<<_:24, P:8, _:32, P:8, _:32, P:8, _:32, P:8, _/binary>>, P) -> true;
win(<<_:32, P:8, _:24, P:8, _:24, P:8, _:24, P:8, _/binary>>, P) -> true;
win(<<_:32, P:8, _:32, P:8, _:32, P:8, _:32, P:8, _/binary>>, P) -> true;
win(<<_:40, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:48, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:80, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:88, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:120, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:128, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(_, _) -> false.

