-module(egambo_tictac_sym_7_6).

-export([ide/1, ver/1, hor/1, pnt/1]).

% generated in egambo_tictac_sym

ide(Board) -> Board.

ver(<<AA:8, AB:8, AC:8, AD:8, AE:8, AF:8, AG:8, BA:8, BB:8, BC:8, BD:8, BE:8, BF:8, BG:8, CA:8, CB:8, CC:8, CD:8, CE:8, CF:8, CG:8, DA:8, DB:8, DC:8, DD:8, DE:8, DF:8, DG:8, EA:8, EB:8, EC:8, ED:8, EE:8, EF:8, EG:8, FA:8, FB:8, FC:8, FD:8, FE:8, FF:8, FG:8>>) -> 
    <<AG:8, AF:8, AE:8, AD:8, AC:8, AB:8, AA:8, BG:8, BF:8, BE:8, BD:8, BC:8, BB:8, BA:8, CG:8, CF:8, CE:8, CD:8, CC:8, CB:8, CA:8, DG:8, DF:8, DE:8, DD:8, DC:8, DB:8, DA:8, EG:8, EF:8, EE:8, ED:8, EC:8, EB:8, EA:8, FG:8, FF:8, FE:8, FD:8, FC:8, FB:8, FA:8>>.

hor(<<AA:8, AB:8, AC:8, AD:8, AE:8, AF:8, AG:8, BA:8, BB:8, BC:8, BD:8, BE:8, BF:8, BG:8, CA:8, CB:8, CC:8, CD:8, CE:8, CF:8, CG:8, DA:8, DB:8, DC:8, DD:8, DE:8, DF:8, DG:8, EA:8, EB:8, EC:8, ED:8, EE:8, EF:8, EG:8, FA:8, FB:8, FC:8, FD:8, FE:8, FF:8, FG:8>>) -> 
    <<FA:8, FB:8, FC:8, FD:8, FE:8, FF:8, FG:8, EA:8, EB:8, EC:8, ED:8, EE:8, EF:8, EG:8, DA:8, DB:8, DC:8, DD:8, DE:8, DF:8, DG:8, CA:8, CB:8, CC:8, CD:8, CE:8, CF:8, CG:8, BA:8, BB:8, BC:8, BD:8, BE:8, BF:8, BG:8, AA:8, AB:8, AC:8, AD:8, AE:8, AF:8, AG:8>>.

pnt(<<AA:8, AB:8, AC:8, AD:8, AE:8, AF:8, AG:8, BA:8, BB:8, BC:8, BD:8, BE:8, BF:8, BG:8, CA:8, CB:8, CC:8, CD:8, CE:8, CF:8, CG:8, DA:8, DB:8, DC:8, DD:8, DE:8, DF:8, DG:8, EA:8, EB:8, EC:8, ED:8, EE:8, EF:8, EG:8, FA:8, FB:8, FC:8, FD:8, FE:8, FF:8, FG:8>>) -> 
    <<FG:8, FF:8, FE:8, FD:8, FC:8, FB:8, FA:8, EG:8, EF:8, EE:8, ED:8, EC:8, EB:8, EA:8, DG:8, DF:8, DE:8, DD:8, DC:8, DB:8, DA:8, CG:8, CF:8, CE:8, CD:8, CC:8, CB:8, CA:8, BG:8, BF:8, BE:8, BD:8, BC:8, BB:8, BA:8, AG:8, AF:8, AE:8, AD:8, AC:8, AB:8, AA:8>>.


