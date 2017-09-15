-module(egambo_tictac_sym_6_6).

-export([a_ide/1, b_ver/1, c_hor/1, d_pnt/1, e_bck/1, f_fwd/1, g_lft/1, h_rgt/1]).

-export([x01/1, x02/1, x03/1, x04/1, x05/1]).

-export([y01/1, y02/1, y03/1, y04/1, y05/1]).

% generated in egambo_tictac_sym

a_ide(Board) -> Board.

b_ver([AA, AB, AC, AD, AE, AF]) -> 
    [AF, AE, AD, AC, AB, AA];

b_ver([AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF, EA, EB, EC, ED, EE, EF, FA, FB, FC, FD, FE, FF]) -> 
    [AF, AE, AD, AC, AB, AA, BF, BE, BD, BC, BB, BA, CF, CE, CD, CC, CB, CA, DF, DE, DD, DC, DB, DA, EF, EE, ED, EC, EB, EA, FF, FE, FD, FC, FB, FA].

c_hor([AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF, EA, EB, EC, ED, EE, EF, FA, FB, FC, FD, FE, FF]) -> 
    [FA, FB, FC, FD, FE, FF, EA, EB, EC, ED, EE, EF, DA, DB, DC, DD, DE, DF, CA, CB, CC, CD, CE, CF, BA, BB, BC, BD, BE, BF, AA, AB, AC, AD, AE, AF].

d_pnt([AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF, EA, EB, EC, ED, EE, EF, FA, FB, FC, FD, FE, FF]) -> 
    [FF, FE, FD, FC, FB, FA, EF, EE, ED, EC, EB, EA, DF, DE, DD, DC, DB, DA, CF, CE, CD, CC, CB, CA, BF, BE, BD, BC, BB, BA, AF, AE, AD, AC, AB, AA].

e_bck([AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF, EA, EB, EC, ED, EE, EF, FA, FB, FC, FD, FE, FF]) -> 
    [AA, BA, CA, DA, EA, FA, AB, BB, CB, DB, EB, FB, AC, BC, CC, DC, EC, FC, AD, BD, CD, DD, ED, FD, AE, BE, CE, DE, EE, FE, AF, BF, CF, DF, EF, FF].

f_fwd([AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF, EA, EB, EC, ED, EE, EF, FA, FB, FC, FD, FE, FF]) -> 
    [FF, EF, DF, CF, BF, AF, FE, EE, DE, CE, BE, AE, FD, ED, DD, CD, BD, AD, FC, EC, DC, CC, BC, AC, FB, EB, DB, CB, BB, AB, FA, EA, DA, CA, BA, AA].

g_lft([AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF, EA, EB, EC, ED, EE, EF, FA, FB, FC, FD, FE, FF]) -> 
    [AF, BF, CF, DF, EF, FF, AE, BE, CE, DE, EE, FE, AD, BD, CD, DD, ED, FD, AC, BC, CC, DC, EC, FC, AB, BB, CB, DB, EB, FB, AA, BA, CA, DA, EA, FA].

h_rgt([AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF, EA, EB, EC, ED, EE, EF, FA, FB, FC, FD, FE, FF]) -> 
    [FA, EA, DA, CA, BA, AA, FB, EB, DB, CB, BB, AB, FC, EC, DC, CC, BC, AC, FD, ED, DD, CD, BD, AD, FE, EE, DE, CE, BE, AE, FF, EF, DF, CF, BF, AF].


x01([AA, AB, AC, AD, AE, AF]) -> 
    [AF, AA, AB, AC, AD, AE];
x01([AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF, EA, EB, EC, ED, EE, EF, FA, FB, FC, FD, FE, FF]) -> 
    [AF, AA, AB, AC, AD, AE, BF, BA, BB, BC, BD, BE, CF, CA, CB, CC, CD, CE, DF, DA, DB, DC, DD, DE, EF, EA, EB, EC, ED, EE, FF, FA, FB, FC, FD, FE].

x02([AA, AB, AC, AD, AE, AF]) -> 
    [AE, AF, AA, AB, AC, AD];
x02([AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF, EA, EB, EC, ED, EE, EF, FA, FB, FC, FD, FE, FF]) -> 
    [AE, AF, AA, AB, AC, AD, BE, BF, BA, BB, BC, BD, CE, CF, CA, CB, CC, CD, DE, DF, DA, DB, DC, DD, EE, EF, EA, EB, EC, ED, FE, FF, FA, FB, FC, FD].

x03([AA, AB, AC, AD, AE, AF]) -> 
    [AD, AE, AF, AA, AB, AC];
x03([AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF, EA, EB, EC, ED, EE, EF, FA, FB, FC, FD, FE, FF]) -> 
    [AD, AE, AF, AA, AB, AC, BD, BE, BF, BA, BB, BC, CD, CE, CF, CA, CB, CC, DD, DE, DF, DA, DB, DC, ED, EE, EF, EA, EB, EC, FD, FE, FF, FA, FB, FC].

x04([AA, AB, AC, AD, AE, AF]) -> 
    [AC, AD, AE, AF, AA, AB];
x04([AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF, EA, EB, EC, ED, EE, EF, FA, FB, FC, FD, FE, FF]) -> 
    [AC, AD, AE, AF, AA, AB, BC, BD, BE, BF, BA, BB, CC, CD, CE, CF, CA, CB, DC, DD, DE, DF, DA, DB, EC, ED, EE, EF, EA, EB, FC, FD, FE, FF, FA, FB].

x05([AA, AB, AC, AD, AE, AF]) -> 
    [AB, AC, AD, AE, AF, AA];
x05([AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF, EA, EB, EC, ED, EE, EF, FA, FB, FC, FD, FE, FF]) -> 
    [AB, AC, AD, AE, AF, AA, BB, BC, BD, BE, BF, BA, CB, CC, CD, CE, CF, CA, DB, DC, DD, DE, DF, DA, EB, EC, ED, EE, EF, EA, FB, FC, FD, FE, FF, FA].


y01([AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF, EA, EB, EC, ED, EE, EF, FA, FB, FC, FD, FE, FF]) -> 
    [FA, FB, FC, FD, FE, FF, AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF, EA, EB, EC, ED, EE, EF].

y02([AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF, EA, EB, EC, ED, EE, EF, FA, FB, FC, FD, FE, FF]) -> 
    [EA, EB, EC, ED, EE, EF, FA, FB, FC, FD, FE, FF, AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF].

y03([AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF, EA, EB, EC, ED, EE, EF, FA, FB, FC, FD, FE, FF]) -> 
    [DA, DB, DC, DD, DE, DF, EA, EB, EC, ED, EE, EF, FA, FB, FC, FD, FE, FF, AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF].

y04([AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF, EA, EB, EC, ED, EE, EF, FA, FB, FC, FD, FE, FF]) -> 
    [CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF, EA, EB, EC, ED, EE, EF, FA, FB, FC, FD, FE, FF, AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF].

y05([AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF, EA, EB, EC, ED, EE, EF, FA, FB, FC, FD, FE, FF]) -> 
    [BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF, EA, EB, EC, ED, EE, EF, FA, FB, FC, FD, FE, FF, AA, AB, AC, AD, AE, AF].


