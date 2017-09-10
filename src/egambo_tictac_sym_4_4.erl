-module(egambo_tictac_sym_4_4).

-export([ide/1, ver/1, hor/1, pnt/1, bck/1, fwd/1, lft/1, rgt/1]).

% generated in egambo_tictac_sym

ide(Board) -> Board.

ver(<<AA:8, AB:8, AC:8, AD:8, BA:8, BB:8, BC:8, BD:8, CA:8, CB:8, CC:8, CD:8, DA:8, DB:8, DC:8, DD:8>>) -> 
    <<AD:8, AC:8, AB:8, AA:8, BD:8, BC:8, BB:8, BA:8, CD:8, CC:8, CB:8, CA:8, DD:8, DC:8, DB:8, DA:8>>.

hor(<<AA:8, AB:8, AC:8, AD:8, BA:8, BB:8, BC:8, BD:8, CA:8, CB:8, CC:8, CD:8, DA:8, DB:8, DC:8, DD:8>>) -> 
    <<DA:8, DB:8, DC:8, DD:8, CA:8, CB:8, CC:8, CD:8, BA:8, BB:8, BC:8, BD:8, AA:8, AB:8, AC:8, AD:8>>.

pnt(<<AA:8, AB:8, AC:8, AD:8, BA:8, BB:8, BC:8, BD:8, CA:8, CB:8, CC:8, CD:8, DA:8, DB:8, DC:8, DD:8>>) -> 
    <<DD:8, DC:8, DB:8, DA:8, CD:8, CC:8, CB:8, CA:8, BD:8, BC:8, BB:8, BA:8, AD:8, AC:8, AB:8, AA:8>>.

bck(<<AA:8, AB:8, AC:8, AD:8, BA:8, BB:8, BC:8, BD:8, CA:8, CB:8, CC:8, CD:8, DA:8, DB:8, DC:8, DD:8>>) -> 
    <<AA:8, BA:8, CA:8, DA:8, AB:8, BB:8, CB:8, DB:8, AC:8, BC:8, CC:8, DC:8, AD:8, BD:8, CD:8, DD:8>>.

fwd(<<AA:8, AB:8, AC:8, AD:8, BA:8, BB:8, BC:8, BD:8, CA:8, CB:8, CC:8, CD:8, DA:8, DB:8, DC:8, DD:8>>) -> 
    <<DD:8, CD:8, BD:8, AD:8, DC:8, CC:8, BC:8, AC:8, DB:8, CB:8, BB:8, AB:8, DA:8, CA:8, BA:8, AA:8>>.

lft(<<AA:8, AB:8, AC:8, AD:8, BA:8, BB:8, BC:8, BD:8, CA:8, CB:8, CC:8, CD:8, DA:8, DB:8, DC:8, DD:8>>) -> 
    <<AD:8, BD:8, CD:8, DD:8, AC:8, BC:8, CC:8, DC:8, AB:8, BB:8, CB:8, DB:8, AA:8, BA:8, CA:8, DA:8>>.

rgt(<<AA:8, AB:8, AC:8, AD:8, BA:8, BB:8, BC:8, BD:8, CA:8, CB:8, CC:8, CD:8, DA:8, DB:8, DC:8, DD:8>>) -> 
    <<DA:8, CA:8, BA:8, AA:8, DB:8, CB:8, BB:8, AB:8, DC:8, CC:8, BC:8, AC:8, DD:8, CD:8, BD:8, AD:8>>.


