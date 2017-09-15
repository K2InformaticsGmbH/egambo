-module(egambo_tictac_sym_5_5).

-export([a_ide/1, b_ver/1, c_hor/1, d_pnt/1, e_bck/1, f_fwd/1, g_lft/1, h_rgt/1]).

-export([x01/1, x02/1, x03/1, x04/1]).

-export([y01/1, y02/1, y03/1, y04/1]).

% generated in egambo_tictac_sym

a_ide(Board) -> Board.

b_ver([AA, AB, AC, AD, AE]) -> 
    [AE, AD, AC, AB, AA];

b_ver([AA, AB, AC, AD, AE, BA, BB, BC, BD, BE, CA, CB, CC, CD, CE, DA, DB, DC, DD, DE, EA, EB, EC, ED, EE]) -> 
    [AE, AD, AC, AB, AA, BE, BD, BC, BB, BA, CE, CD, CC, CB, CA, DE, DD, DC, DB, DA, EE, ED, EC, EB, EA].

c_hor([AA, AB, AC, AD, AE, BA, BB, BC, BD, BE, CA, CB, CC, CD, CE, DA, DB, DC, DD, DE, EA, EB, EC, ED, EE]) -> 
    [EA, EB, EC, ED, EE, DA, DB, DC, DD, DE, CA, CB, CC, CD, CE, BA, BB, BC, BD, BE, AA, AB, AC, AD, AE].

d_pnt([AA, AB, AC, AD, AE, BA, BB, BC, BD, BE, CA, CB, CC, CD, CE, DA, DB, DC, DD, DE, EA, EB, EC, ED, EE]) -> 
    [EE, ED, EC, EB, EA, DE, DD, DC, DB, DA, CE, CD, CC, CB, CA, BE, BD, BC, BB, BA, AE, AD, AC, AB, AA].

e_bck([AA, AB, AC, AD, AE, BA, BB, BC, BD, BE, CA, CB, CC, CD, CE, DA, DB, DC, DD, DE, EA, EB, EC, ED, EE]) -> 
    [AA, BA, CA, DA, EA, AB, BB, CB, DB, EB, AC, BC, CC, DC, EC, AD, BD, CD, DD, ED, AE, BE, CE, DE, EE].

f_fwd([AA, AB, AC, AD, AE, BA, BB, BC, BD, BE, CA, CB, CC, CD, CE, DA, DB, DC, DD, DE, EA, EB, EC, ED, EE]) -> 
    [EE, DE, CE, BE, AE, ED, DD, CD, BD, AD, EC, DC, CC, BC, AC, EB, DB, CB, BB, AB, EA, DA, CA, BA, AA].

g_lft([AA, AB, AC, AD, AE, BA, BB, BC, BD, BE, CA, CB, CC, CD, CE, DA, DB, DC, DD, DE, EA, EB, EC, ED, EE]) -> 
    [AE, BE, CE, DE, EE, AD, BD, CD, DD, ED, AC, BC, CC, DC, EC, AB, BB, CB, DB, EB, AA, BA, CA, DA, EA].

h_rgt([AA, AB, AC, AD, AE, BA, BB, BC, BD, BE, CA, CB, CC, CD, CE, DA, DB, DC, DD, DE, EA, EB, EC, ED, EE]) -> 
    [EA, DA, CA, BA, AA, EB, DB, CB, BB, AB, EC, DC, CC, BC, AC, ED, DD, CD, BD, AD, EE, DE, CE, BE, AE].


x01([AA, AB, AC, AD, AE]) -> 
    [AE, AA, AB, AC, AD];
x01([AA, AB, AC, AD, AE, BA, BB, BC, BD, BE, CA, CB, CC, CD, CE, DA, DB, DC, DD, DE, EA, EB, EC, ED, EE]) -> 
    [AE, AA, AB, AC, AD, BE, BA, BB, BC, BD, CE, CA, CB, CC, CD, DE, DA, DB, DC, DD, EE, EA, EB, EC, ED].

x02([AA, AB, AC, AD, AE]) -> 
    [AD, AE, AA, AB, AC];
x02([AA, AB, AC, AD, AE, BA, BB, BC, BD, BE, CA, CB, CC, CD, CE, DA, DB, DC, DD, DE, EA, EB, EC, ED, EE]) -> 
    [AD, AE, AA, AB, AC, BD, BE, BA, BB, BC, CD, CE, CA, CB, CC, DD, DE, DA, DB, DC, ED, EE, EA, EB, EC].

x03([AA, AB, AC, AD, AE]) -> 
    [AC, AD, AE, AA, AB];
x03([AA, AB, AC, AD, AE, BA, BB, BC, BD, BE, CA, CB, CC, CD, CE, DA, DB, DC, DD, DE, EA, EB, EC, ED, EE]) -> 
    [AC, AD, AE, AA, AB, BC, BD, BE, BA, BB, CC, CD, CE, CA, CB, DC, DD, DE, DA, DB, EC, ED, EE, EA, EB].

x04([AA, AB, AC, AD, AE]) -> 
    [AB, AC, AD, AE, AA];
x04([AA, AB, AC, AD, AE, BA, BB, BC, BD, BE, CA, CB, CC, CD, CE, DA, DB, DC, DD, DE, EA, EB, EC, ED, EE]) -> 
    [AB, AC, AD, AE, AA, BB, BC, BD, BE, BA, CB, CC, CD, CE, CA, DB, DC, DD, DE, DA, EB, EC, ED, EE, EA].


y01([AA, AB, AC, AD, AE, BA, BB, BC, BD, BE, CA, CB, CC, CD, CE, DA, DB, DC, DD, DE, EA, EB, EC, ED, EE]) -> 
    [EA, EB, EC, ED, EE, AA, AB, AC, AD, AE, BA, BB, BC, BD, BE, CA, CB, CC, CD, CE, DA, DB, DC, DD, DE].

y02([AA, AB, AC, AD, AE, BA, BB, BC, BD, BE, CA, CB, CC, CD, CE, DA, DB, DC, DD, DE, EA, EB, EC, ED, EE]) -> 
    [DA, DB, DC, DD, DE, EA, EB, EC, ED, EE, AA, AB, AC, AD, AE, BA, BB, BC, BD, BE, CA, CB, CC, CD, CE].

y03([AA, AB, AC, AD, AE, BA, BB, BC, BD, BE, CA, CB, CC, CD, CE, DA, DB, DC, DD, DE, EA, EB, EC, ED, EE]) -> 
    [CA, CB, CC, CD, CE, DA, DB, DC, DD, DE, EA, EB, EC, ED, EE, AA, AB, AC, AD, AE, BA, BB, BC, BD, BE].

y04([AA, AB, AC, AD, AE, BA, BB, BC, BD, BE, CA, CB, CC, CD, CE, DA, DB, DC, DD, DE, EA, EB, EC, ED, EE]) -> 
    [BA, BB, BC, BD, BE, CA, CB, CC, CD, CE, DA, DB, DC, DD, DE, EA, EB, EC, ED, EE, AA, AB, AC, AD, AE].


