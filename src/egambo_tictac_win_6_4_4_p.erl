-module(egambo_tictac_win_6_4_4_p).

-export([win/2]).

% generated in egambo_tictac_wip

win(<<P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<P:8, P:8, P:8, _:16, P:8, _/binary>>, P) -> true;
win(<<P:8, P:8, _:16, P:8, P:8, _/binary>>, P) -> true;
win(<<P:8, _:16, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<P:8, _:40, P:8, _:40, P:8, _:40, P:8, _/binary>>, P) -> true;
win(<<P:8, _:48, P:8, _:48, P:8, _:48, P:8, _/binary>>, P) -> true;
win(<<P:8, _:48, P:8, _:48, P:8, _:64, P:8, _/binary>>, P) -> true;
win(<<P:8, _:48, P:8, _:64, P:8, _:48, P:8, _/binary>>, P) -> true;
win(<<P:8, _:64, P:8, _:32, P:8, _:32, P:8, _/binary>>, P) -> true;
win(<<P:8, _:64, P:8, _:48, P:8, _:48, P:8, _/binary>>, P) -> true;
win(<<P:8, _:80, P:8, _:16, P:8, _:32, P:8, _/binary>>, P) -> true;
win(<<P:8, _:80, P:8, _:32, P:8, _:16, P:8, _/binary>>, P) -> true;
win(<<P:8, _:80, P:8, _:32, P:8, _:32, P:8, _/binary>>, P) -> true;
win(<<_:8, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:8, P:8, _:32, P:8, _:64, P:8, _:32, P:8, _/binary>>, P) -> true;
win(<<_:8, P:8, _:32, P:8, _:80, P:8, _:16, P:8, _/binary>>, P) -> true;
win(<<_:8, P:8, _:32, P:8, _:80, P:8, _:32, P:8, _/binary>>, P) -> true;
win(<<_:8, P:8, _:40, P:8, _:40, P:8, _:40, P:8, _/binary>>, P) -> true;
win(<<_:8, P:8, _:48, P:8, _:48, P:8, _:16, P:8, _/binary>>, P) -> true;
win(<<_:8, P:8, _:48, P:8, _:48, P:8, _:48, P:8, _/binary>>, P) -> true;
win(<<_:8, P:8, _:48, P:8, _:64, P:8, P:8, _/binary>>, P) -> true;
win(<<_:8, P:8, _:64, P:8, _:32, P:8, _:32, P:8, _/binary>>, P) -> true;
win(<<_:8, P:8, _:64, P:8, _:48, P:8, P:8, _/binary>>, P) -> true;
win(<<_:16, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:16, P:8, _:32, P:8, _:32, P:8, _:64, P:8, _/binary>>, P) -> true;
win(<<_:16, P:8, _:32, P:8, _:32, P:8, _:80, P:8, _/binary>>, P) -> true;
win(<<_:16, P:8, _:32, P:8, _:64, P:8, _:32, P:8, _/binary>>, P) -> true;
win(<<_:16, P:8, _:40, P:8, _:40, P:8, _:40, P:8, _/binary>>, P) -> true;
win(<<_:16, P:8, _:48, P:8, _:16, P:8, _:48, P:8, _/binary>>, P) -> true;
win(<<_:16, P:8, _:48, P:8, _:48, P:8, _:16, P:8, _/binary>>, P) -> true;
win(<<_:16, P:8, _:48, P:8, _:48, P:8, _:48, P:8, _/binary>>, P) -> true;
win(<<_:16, P:8, _:64, P:8, P:8, _:48, P:8, _/binary>>, P) -> true;
win(<<_:16, P:8, _:64, P:8, _:32, P:8, _:32, P:8, _/binary>>, P) -> true;
win(<<_:24, P:8, _:16, P:8, _:48, P:8, _:48, P:8, _/binary>>, P) -> true;
win(<<_:24, P:8, _:16, P:8, _:80, P:8, _:32, P:8, _/binary>>, P) -> true;
win(<<_:24, P:8, _:32, P:8, _:32, P:8, _:32, P:8, _/binary>>, P) -> true;
win(<<_:24, P:8, _:32, P:8, _:32, P:8, _:64, P:8, _/binary>>, P) -> true;
win(<<_:24, P:8, _:32, P:8, _:64, P:8, _:32, P:8, _/binary>>, P) -> true;
win(<<_:24, P:8, _:40, P:8, _:40, P:8, _:40, P:8, _/binary>>, P) -> true;
win(<<_:24, P:8, _:48, P:8, _:16, P:8, _:48, P:8, _/binary>>, P) -> true;
win(<<_:24, P:8, _:48, P:8, _:48, P:8, P:8, _/binary>>, P) -> true;
win(<<_:24, P:8, _:48, P:8, _:48, P:8, _:16, P:8, _/binary>>, P) -> true;
win(<<_:32, P:8, _:16, P:8, _:32, P:8, _:80, P:8, _/binary>>, P) -> true;
win(<<_:32, P:8, _:16, P:8, _:48, P:8, _:48, P:8, _/binary>>, P) -> true;
win(<<_:32, P:8, _:32, P:8, _:16, P:8, _:80, P:8, _/binary>>, P) -> true;
win(<<_:32, P:8, _:32, P:8, _:32, P:8, _:32, P:8, _/binary>>, P) -> true;
win(<<_:32, P:8, _:32, P:8, _:32, P:8, _:64, P:8, _/binary>>, P) -> true;
win(<<_:32, P:8, _:40, P:8, _:40, P:8, _:40, P:8, _/binary>>, P) -> true;
win(<<_:32, P:8, _:48, P:8, P:8, _:48, P:8, _/binary>>, P) -> true;
win(<<_:32, P:8, _:48, P:8, P:8, _:64, P:8, _/binary>>, P) -> true;
win(<<_:32, P:8, _:48, P:8, _:16, P:8, _:48, P:8, _/binary>>, P) -> true;
win(<<_:40, P:8, P:8, _:48, P:8, _:48, P:8, _/binary>>, P) -> true;
win(<<_:40, P:8, P:8, _:48, P:8, _:64, P:8, _/binary>>, P) -> true;
win(<<_:40, P:8, P:8, _:64, P:8, _:48, P:8, _/binary>>, P) -> true;
win(<<_:40, P:8, _:16, P:8, _:32, P:8, _:32, P:8, _/binary>>, P) -> true;
win(<<_:40, P:8, _:16, P:8, _:48, P:8, _:48, P:8, _/binary>>, P) -> true;
win(<<_:40, P:8, _:32, P:8, _:16, P:8, _:32, P:8, _/binary>>, P) -> true;
win(<<_:40, P:8, _:32, P:8, _:32, P:8, _:16, P:8, _/binary>>, P) -> true;
win(<<_:40, P:8, _:32, P:8, _:32, P:8, _:32, P:8, _/binary>>, P) -> true;
win(<<_:40, P:8, _:40, P:8, _:40, P:8, _:40, P:8, _/binary>>, P) -> true;
win(<<_:48, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:48, P:8, P:8, P:8, _:16, P:8, _/binary>>, P) -> true;
win(<<_:48, P:8, P:8, _:16, P:8, P:8, _/binary>>, P) -> true;
win(<<_:48, P:8, _:16, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:56, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:64, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:96, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:96, P:8, P:8, P:8, _:16, P:8, _/binary>>, P) -> true;
win(<<_:96, P:8, P:8, _:16, P:8, P:8, _/binary>>, P) -> true;
win(<<_:96, P:8, _:16, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:104, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:112, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:144, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:144, P:8, P:8, P:8, _:16, P:8, _/binary>>, P) -> true;
win(<<_:144, P:8, P:8, _:16, P:8, P:8, _/binary>>, P) -> true;
win(<<_:144, P:8, _:16, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:152, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:160, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(_, _) -> false.

