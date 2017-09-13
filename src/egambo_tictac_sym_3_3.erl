-module(egambo_tictac_sym_3_3).

-export([ide/1, ver/1, hor/1, pnt/1, bck/1, fwd/1, lft/1, rgt/1]).

-export([x01/1, x02/1]).

-export([y01/1, y02/1]).

% generated in egambo_tictac_sym

ide(Board) -> Board.

ver([AA, AB, AC]) -> 
    [AC, AB, AA];

ver([AA, AB, AC, BA, BB, BC, CA, CB, CC]) -> 
    [AC, AB, AA, BC, BB, BA, CC, CB, CA].

hor([AA, AB, AC, BA, BB, BC, CA, CB, CC]) -> 
    [CA, CB, CC, BA, BB, BC, AA, AB, AC].

pnt([AA, AB, AC, BA, BB, BC, CA, CB, CC]) -> 
    [CC, CB, CA, BC, BB, BA, AC, AB, AA].

bck([AA, AB, AC, BA, BB, BC, CA, CB, CC]) -> 
    [AA, BA, CA, AB, BB, CB, AC, BC, CC].

fwd([AA, AB, AC, BA, BB, BC, CA, CB, CC]) -> 
    [CC, BC, AC, CB, BB, AB, CA, BA, AA].

lft([AA, AB, AC, BA, BB, BC, CA, CB, CC]) -> 
    [AC, BC, CC, AB, BB, CB, AA, BA, CA].

rgt([AA, AB, AC, BA, BB, BC, CA, CB, CC]) -> 
    [CA, BA, AA, CB, BB, AB, CC, BC, AC].


x01([AA, AB, AC]) -> 
    [AB, AC, AA];
x01([AA, AB, AC, BA, BB, BC, CA, CB, CC]) -> 
    [AB, AC, AA, BB, BC, BA, CB, CC, CA].

x02([AA, AB, AC]) -> 
    [AC, AA, AB];
x02([AA, AB, AC, BA, BB, BC, CA, CB, CC]) -> 
    [AC, AA, AB, BC, BA, BB, CC, CA, CB].


y01([AA, AB, AC, BA, BB, BC, CA, CB, CC]) -> 
    [BA, BB, BC, CA, CB, CC, AA, AB, AC].

y02([AA, AB, AC, BA, BB, BC, CA, CB, CC]) -> 
    [CA, CB, CC, AA, AB, AC, BA, BB, BC].


