-module(egambo_tictac_sym_5_5).

-export([ide/1, ver/1, hor/1, pnt/1, bck/1, fwd/1, lft/1, rgt/1]).

% generated in egambo_tictac_sym

ide(Board) -> Board.

ver(<<AA:8, AB:8, AC:8, AD:8, AE:8, BA:8, BB:8, BC:8, BD:8, BE:8, CA:8, CB:8, CC:8, CD:8, CE:8, DA:8, DB:8, DC:8, DD:8, DE:8, EA:8, EB:8, EC:8, ED:8, EE:8>>) -> 
    <<AE:8, AD:8, AC:8, AB:8, AA:8, BE:8, BD:8, BC:8, BB:8, BA:8, CE:8, CD:8, CC:8, CB:8, CA:8, DE:8, DD:8, DC:8, DB:8, DA:8, EE:8, ED:8, EC:8, EB:8, EA:8>>.

hor(<<AA:8, AB:8, AC:8, AD:8, AE:8, BA:8, BB:8, BC:8, BD:8, BE:8, CA:8, CB:8, CC:8, CD:8, CE:8, DA:8, DB:8, DC:8, DD:8, DE:8, EA:8, EB:8, EC:8, ED:8, EE:8>>) -> 
    <<EA:8, EB:8, EC:8, ED:8, EE:8, DA:8, DB:8, DC:8, DD:8, DE:8, CA:8, CB:8, CC:8, CD:8, CE:8, BA:8, BB:8, BC:8, BD:8, BE:8, AA:8, AB:8, AC:8, AD:8, AE:8>>.

pnt(<<AA:8, AB:8, AC:8, AD:8, AE:8, BA:8, BB:8, BC:8, BD:8, BE:8, CA:8, CB:8, CC:8, CD:8, CE:8, DA:8, DB:8, DC:8, DD:8, DE:8, EA:8, EB:8, EC:8, ED:8, EE:8>>) -> 
    <<EE:8, ED:8, EC:8, EB:8, EA:8, DE:8, DD:8, DC:8, DB:8, DA:8, CE:8, CD:8, CC:8, CB:8, CA:8, BE:8, BD:8, BC:8, BB:8, BA:8, AE:8, AD:8, AC:8, AB:8, AA:8>>.

bck(<<AA:8, AB:8, AC:8, AD:8, AE:8, BA:8, BB:8, BC:8, BD:8, BE:8, CA:8, CB:8, CC:8, CD:8, CE:8, DA:8, DB:8, DC:8, DD:8, DE:8, EA:8, EB:8, EC:8, ED:8, EE:8>>) -> 
    <<AA:8, BA:8, CA:8, DA:8, EA:8, AB:8, BB:8, CB:8, DB:8, EB:8, AC:8, BC:8, CC:8, DC:8, EC:8, AD:8, BD:8, CD:8, DD:8, ED:8, AE:8, BE:8, CE:8, DE:8, EE:8>>.

fwd(<<AA:8, AB:8, AC:8, AD:8, AE:8, BA:8, BB:8, BC:8, BD:8, BE:8, CA:8, CB:8, CC:8, CD:8, CE:8, DA:8, DB:8, DC:8, DD:8, DE:8, EA:8, EB:8, EC:8, ED:8, EE:8>>) -> 
    <<EE:8, DE:8, CE:8, BE:8, AE:8, ED:8, DD:8, CD:8, BD:8, AD:8, EC:8, DC:8, CC:8, BC:8, AC:8, EB:8, DB:8, CB:8, BB:8, AB:8, EA:8, DA:8, CA:8, BA:8, AA:8>>.

lft(<<AA:8, AB:8, AC:8, AD:8, AE:8, BA:8, BB:8, BC:8, BD:8, BE:8, CA:8, CB:8, CC:8, CD:8, CE:8, DA:8, DB:8, DC:8, DD:8, DE:8, EA:8, EB:8, EC:8, ED:8, EE:8>>) -> 
    <<AE:8, BE:8, CE:8, DE:8, EE:8, AD:8, BD:8, CD:8, DD:8, ED:8, AC:8, BC:8, CC:8, DC:8, EC:8, AB:8, BB:8, CB:8, DB:8, EB:8, AA:8, BA:8, CA:8, DA:8, EA:8>>.

rgt(<<AA:8, AB:8, AC:8, AD:8, AE:8, BA:8, BB:8, BC:8, BD:8, BE:8, CA:8, CB:8, CC:8, CD:8, CE:8, DA:8, DB:8, DC:8, DD:8, DE:8, EA:8, EB:8, EC:8, ED:8, EE:8>>) -> 
    <<EA:8, DA:8, CA:8, BA:8, AA:8, EB:8, DB:8, CB:8, BB:8, AB:8, EC:8, DC:8, CC:8, BC:8, AC:8, ED:8, DD:8, CD:8, BD:8, AD:8, EE:8, DE:8, CE:8, BE:8, AE:8>>.


