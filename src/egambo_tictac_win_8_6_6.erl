-module(egambo_tictac_win_8_6_6).

-export([win/2]).

% generated in egambo_tictac_win

win(<<P:8, P:8, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<P:8, _:56, P:8, _:56, P:8, _:56, P:8, _:56, P:8, _:56, P:8, _/binary>>, P) -> true;
win(<<P:8, _:64, P:8, _:64, P:8, _:64, P:8, _:64, P:8, _:64, P:8, _/binary>>, P) -> true;
win(<<_:8, P:8, P:8, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:8, P:8, _:56, P:8, _:56, P:8, _:56, P:8, _:56, P:8, _:56, P:8, _/binary>>, P) -> true;
win(<<_:8, P:8, _:64, P:8, _:64, P:8, _:64, P:8, _:64, P:8, _:64, P:8, _/binary>>, P) -> true;
win(<<_:16, P:8, P:8, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:16, P:8, _:56, P:8, _:56, P:8, _:56, P:8, _:56, P:8, _:56, P:8, _/binary>>, P) -> true;
win(<<_:16, P:8, _:64, P:8, _:64, P:8, _:64, P:8, _:64, P:8, _:64, P:8, _/binary>>, P) -> true;
win(<<_:24, P:8, _:56, P:8, _:56, P:8, _:56, P:8, _:56, P:8, _:56, P:8, _/binary>>, P) -> true;
win(<<_:32, P:8, _:56, P:8, _:56, P:8, _:56, P:8, _:56, P:8, _:56, P:8, _/binary>>, P) -> true;
win(<<_:40, P:8, _:48, P:8, _:48, P:8, _:48, P:8, _:48, P:8, _:48, P:8, _/binary>>, P) -> true;
win(<<_:40, P:8, _:56, P:8, _:56, P:8, _:56, P:8, _:56, P:8, _:56, P:8, _/binary>>, P) -> true;
win(<<_:48, P:8, _:48, P:8, _:48, P:8, _:48, P:8, _:48, P:8, _:48, P:8, _/binary>>, P) -> true;
win(<<_:48, P:8, _:56, P:8, _:56, P:8, _:56, P:8, _:56, P:8, _:56, P:8, _/binary>>, P) -> true;
win(<<_:56, P:8, _:48, P:8, _:48, P:8, _:48, P:8, _:48, P:8, _:48, P:8, _/binary>>, P) -> true;
win(<<_:56, P:8, _:56, P:8, _:56, P:8, _:56, P:8, _:56, P:8, _:56, P:8, _/binary>>, P) -> true;
win(<<_:64, P:8, P:8, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:72, P:8, P:8, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:80, P:8, P:8, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:128, P:8, P:8, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:136, P:8, P:8, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:144, P:8, P:8, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:192, P:8, P:8, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:200, P:8, P:8, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:208, P:8, P:8, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:256, P:8, P:8, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:264, P:8, P:8, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:272, P:8, P:8, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:320, P:8, P:8, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:328, P:8, P:8, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(<<_:336, P:8, P:8, P:8, P:8, P:8, P:8, _/binary>>, P) -> true;
win(_, _) -> false.

