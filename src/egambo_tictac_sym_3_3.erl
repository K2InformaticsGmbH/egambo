-module(egambo_tictac_sym_3_3).

-export([ide/1, ver/1, hor/1, pnt/1, bck/1, fwd/1, lft/1, rgt/1]).

% generated in egambo_tictac_sym

ide(Board) -> Board.

ver(<<AA:8, AB:8, AC:8, BA:8, BB:8, BC:8, CA:8, CB:8, CC:8>>) -> 
    <<AC:8, AB:8, AA:8, BC:8, BB:8, BA:8, CC:8, CB:8, CA:8>>.

hor(<<AA:8, AB:8, AC:8, BA:8, BB:8, BC:8, CA:8, CB:8, CC:8>>) -> 
    <<CA:8, CB:8, CC:8, BA:8, BB:8, BC:8, AA:8, AB:8, AC:8>>.

pnt(<<AA:8, AB:8, AC:8, BA:8, BB:8, BC:8, CA:8, CB:8, CC:8>>) -> 
    <<CC:8, CB:8, CA:8, BC:8, BB:8, BA:8, AC:8, AB:8, AA:8>>.

bck(<<AA:8, AB:8, AC:8, BA:8, BB:8, BC:8, CA:8, CB:8, CC:8>>) -> 
    <<AA:8, BA:8, CA:8, AB:8, BB:8, CB:8, AC:8, BC:8, CC:8>>.

fwd(<<AA:8, AB:8, AC:8, BA:8, BB:8, BC:8, CA:8, CB:8, CC:8>>) -> 
    <<CC:8, BC:8, AC:8, CB:8, BB:8, AB:8, CA:8, BA:8, AA:8>>.

lft(<<AA:8, AB:8, AC:8, BA:8, BB:8, BC:8, CA:8, CB:8, CC:8>>) -> 
    <<AC:8, BC:8, CC:8, AB:8, BB:8, CB:8, AA:8, BA:8, CA:8>>.

rgt(<<AA:8, AB:8, AC:8, BA:8, BB:8, BC:8, CA:8, CB:8, CC:8>>) -> 
    <<CA:8, BA:8, AA:8, CB:8, BB:8, AB:8, CC:8, BC:8, AC:8>>.


