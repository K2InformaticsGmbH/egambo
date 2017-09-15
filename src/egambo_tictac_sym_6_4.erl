-module(egambo_tictac_sym_6_4).

-export([a_ide/1, b_ver/1, c_hor/1, d_pnt/1]).

-export([x01/1, x02/1, x03/1, x04/1, x05/1]).

-export([y01/1, y02/1, y03/1]).

% generated in egambo_tictac_sym

a_ide(Board) -> Board.

b_ver([AA, AB, AC, AD, AE, AF]) -> 
    [AF, AE, AD, AC, AB, AA];

b_ver([AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF]) -> 
    [AF, AE, AD, AC, AB, AA, BF, BE, BD, BC, BB, BA, CF, CE, CD, CC, CB, CA, DF, DE, DD, DC, DB, DA].

c_hor([AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF]) -> 
    [DA, DB, DC, DD, DE, DF, CA, CB, CC, CD, CE, CF, BA, BB, BC, BD, BE, BF, AA, AB, AC, AD, AE, AF].

d_pnt([AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF]) -> 
    [DF, DE, DD, DC, DB, DA, CF, CE, CD, CC, CB, CA, BF, BE, BD, BC, BB, BA, AF, AE, AD, AC, AB, AA].


x01([AA, AB, AC, AD, AE, AF]) -> 
    [AF, AA, AB, AC, AD, AE];
x01([AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF]) -> 
    [AF, AA, AB, AC, AD, AE, BF, BA, BB, BC, BD, BE, CF, CA, CB, CC, CD, CE, DF, DA, DB, DC, DD, DE].

x02([AA, AB, AC, AD, AE, AF]) -> 
    [AE, AF, AA, AB, AC, AD];
x02([AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF]) -> 
    [AE, AF, AA, AB, AC, AD, BE, BF, BA, BB, BC, BD, CE, CF, CA, CB, CC, CD, DE, DF, DA, DB, DC, DD].

x03([AA, AB, AC, AD, AE, AF]) -> 
    [AD, AE, AF, AA, AB, AC];
x03([AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF]) -> 
    [AD, AE, AF, AA, AB, AC, BD, BE, BF, BA, BB, BC, CD, CE, CF, CA, CB, CC, DD, DE, DF, DA, DB, DC].

x04([AA, AB, AC, AD, AE, AF]) -> 
    [AC, AD, AE, AF, AA, AB];
x04([AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF]) -> 
    [AC, AD, AE, AF, AA, AB, BC, BD, BE, BF, BA, BB, CC, CD, CE, CF, CA, CB, DC, DD, DE, DF, DA, DB].

x05([AA, AB, AC, AD, AE, AF]) -> 
    [AB, AC, AD, AE, AF, AA];
x05([AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF]) -> 
    [AB, AC, AD, AE, AF, AA, BB, BC, BD, BE, BF, BA, CB, CC, CD, CE, CF, CA, DB, DC, DD, DE, DF, DA].


y01([AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF]) -> 
    [DA, DB, DC, DD, DE, DF, AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF].

y02([AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF]) -> 
    [CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF, AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF].

y03([AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF]) -> 
    [BA, BB, BC, BD, BE, BF, CA, CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF, AA, AB, AC, AD, AE, AF].


