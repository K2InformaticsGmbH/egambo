-module(egambo_tictac_sym_4_4).

-export([a_ide/1, b_ver/1, c_hor/1, d_pnt/1, e_bck/1, f_fwd/1, g_lft/1, h_rgt/1]).

-export([x01/1, x02/1, x03/1]).

-export([y01/1, y02/1, y03/1]).

% generated in egambo_tictac_sym

a_ide(Board) -> Board.

b_ver([AA, AB, AC, AD]) -> 
    [AD, AC, AB, AA];

b_ver([AA, AB, AC, AD, BA, BB, BC, BD, CA, CB, CC, CD, DA, DB, DC, DD]) -> 
    [AD, AC, AB, AA, BD, BC, BB, BA, CD, CC, CB, CA, DD, DC, DB, DA].

c_hor([AA, AB, AC, AD, BA, BB, BC, BD, CA, CB, CC, CD, DA, DB, DC, DD]) -> 
    [DA, DB, DC, DD, CA, CB, CC, CD, BA, BB, BC, BD, AA, AB, AC, AD].

d_pnt([AA, AB, AC, AD, BA, BB, BC, BD, CA, CB, CC, CD, DA, DB, DC, DD]) -> 
    [DD, DC, DB, DA, CD, CC, CB, CA, BD, BC, BB, BA, AD, AC, AB, AA].

e_bck([AA, AB, AC, AD, BA, BB, BC, BD, CA, CB, CC, CD, DA, DB, DC, DD]) -> 
    [AA, BA, CA, DA, AB, BB, CB, DB, AC, BC, CC, DC, AD, BD, CD, DD].

f_fwd([AA, AB, AC, AD, BA, BB, BC, BD, CA, CB, CC, CD, DA, DB, DC, DD]) -> 
    [DD, CD, BD, AD, DC, CC, BC, AC, DB, CB, BB, AB, DA, CA, BA, AA].

g_lft([AA, AB, AC, AD, BA, BB, BC, BD, CA, CB, CC, CD, DA, DB, DC, DD]) -> 
    [AD, BD, CD, DD, AC, BC, CC, DC, AB, BB, CB, DB, AA, BA, CA, DA].

h_rgt([AA, AB, AC, AD, BA, BB, BC, BD, CA, CB, CC, CD, DA, DB, DC, DD]) -> 
    [DA, CA, BA, AA, DB, CB, BB, AB, DC, CC, BC, AC, DD, CD, BD, AD].


x01([AA, AB, AC, AD]) -> 
    [AD, AA, AB, AC];
x01([AA, AB, AC, AD, BA, BB, BC, BD, CA, CB, CC, CD, DA, DB, DC, DD]) -> 
    [AD, AA, AB, AC, BD, BA, BB, BC, CD, CA, CB, CC, DD, DA, DB, DC].

x02([AA, AB, AC, AD]) -> 
    [AC, AD, AA, AB];
x02([AA, AB, AC, AD, BA, BB, BC, BD, CA, CB, CC, CD, DA, DB, DC, DD]) -> 
    [AC, AD, AA, AB, BC, BD, BA, BB, CC, CD, CA, CB, DC, DD, DA, DB].

x03([AA, AB, AC, AD]) -> 
    [AB, AC, AD, AA];
x03([AA, AB, AC, AD, BA, BB, BC, BD, CA, CB, CC, CD, DA, DB, DC, DD]) -> 
    [AB, AC, AD, AA, BB, BC, BD, BA, CB, CC, CD, CA, DB, DC, DD, DA].


y01([AA, AB, AC, AD, BA, BB, BC, BD, CA, CB, CC, CD, DA, DB, DC, DD]) -> 
    [DA, DB, DC, DD, AA, AB, AC, AD, BA, BB, BC, BD, CA, CB, CC, CD].

y02([AA, AB, AC, AD, BA, BB, BC, BD, CA, CB, CC, CD, DA, DB, DC, DD]) -> 
    [CA, CB, CC, CD, DA, DB, DC, DD, AA, AB, AC, AD, BA, BB, BC, BD].

y03([AA, AB, AC, AD, BA, BB, BC, BD, CA, CB, CC, CD, DA, DB, DC, DD]) -> 
    [BA, BB, BC, BD, CA, CB, CC, CD, DA, DB, DC, DD, AA, AB, AC, AD].


