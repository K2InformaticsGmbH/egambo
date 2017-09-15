-module(egambo_tictac_sym_3_3).

-export([a_ide/1, b_ver/1, c_hor/1, d_pnt/1, e_bck/1, f_fwd/1, g_lft/1, h_rgt/1]).

-export([x01/1, x02/1]).

-export([y01/1, y02/1]).

% generated in egambo_tictac_sym

a_ide(Board) -> Board.

b_ver([AA, AB, AC]) -> 
    [AC, AB, AA];

b_ver([AA, AB, AC, BA, BB, BC, CA, CB, CC]) -> 
    [AC, AB, AA, BC, BB, BA, CC, CB, CA].

c_hor([AA, AB, AC, BA, BB, BC, CA, CB, CC]) -> 
    [CA, CB, CC, BA, BB, BC, AA, AB, AC].

d_pnt([AA, AB, AC, BA, BB, BC, CA, CB, CC]) -> 
    [CC, CB, CA, BC, BB, BA, AC, AB, AA].

e_bck([AA, AB, AC, BA, BB, BC, CA, CB, CC]) -> 
    [AA, BA, CA, AB, BB, CB, AC, BC, CC].

f_fwd([AA, AB, AC, BA, BB, BC, CA, CB, CC]) -> 
    [CC, BC, AC, CB, BB, AB, CA, BA, AA].

g_lft([AA, AB, AC, BA, BB, BC, CA, CB, CC]) -> 
    [AC, BC, CC, AB, BB, CB, AA, BA, CA].

h_rgt([AA, AB, AC, BA, BB, BC, CA, CB, CC]) -> 
    [CA, BA, AA, CB, BB, AB, CC, BC, AC].


x01([AA, AB, AC]) -> 
    [AC, AA, AB];
x01([AA, AB, AC, BA, BB, BC, CA, CB, CC]) -> 
    [AC, AA, AB, BC, BA, BB, CC, CA, CB].

x02([AA, AB, AC]) -> 
    [AB, AC, AA];
x02([AA, AB, AC, BA, BB, BC, CA, CB, CC]) -> 
    [AB, AC, AA, BB, BC, BA, CB, CC, CA].


y01([AA, AB, AC, BA, BB, BC, CA, CB, CC]) -> 
    [CA, CB, CC, AA, AB, AC, BA, BB, BC].

y02([AA, AB, AC, BA, BB, BC, CA, CB, CC]) -> 
    [BA, BB, BC, CA, CB, CC, AA, AB, AC].


