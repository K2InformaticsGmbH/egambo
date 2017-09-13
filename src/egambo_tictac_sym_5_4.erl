-module(egambo_tictac_sym_5_4).

-export([ide/1, ver/1, hor/1, pnt/1]).

-export([x01/1, x02/1, x03/1, x04/1]).

-export([y01/1, y02/1, y03/1]).

% generated in egambo_tictac_sym

ide(Board) -> Board.

ver([AA, AB, AC, AD, AE]) -> 
    [AE, AD, AC, AB, AA];

ver([AA, AB, AC, AD, AE, BA, BB, BC, BD, BE, CA, CB, CC, CD, CE, DA, DB, DC, DD, DE]) -> 
    [AE, AD, AC, AB, AA, BE, BD, BC, BB, BA, CE, CD, CC, CB, CA, DE, DD, DC, DB, DA].

hor([AA, AB, AC, AD, AE, BA, BB, BC, BD, BE, CA, CB, CC, CD, CE, DA, DB, DC, DD, DE]) -> 
    [DA, DB, DC, DD, DE, CA, CB, CC, CD, CE, BA, BB, BC, BD, BE, AA, AB, AC, AD, AE].

pnt([AA, AB, AC, AD, AE, BA, BB, BC, BD, BE, CA, CB, CC, CD, CE, DA, DB, DC, DD, DE]) -> 
    [DE, DD, DC, DB, DA, CE, CD, CC, CB, CA, BE, BD, BC, BB, BA, AE, AD, AC, AB, AA].


x01([AA, AB, AC, AD, AE]) -> 
    [AB, AC, AD, AE, AA];
x01([AA, AB, AC, AD, AE, BA, BB, BC, BD, BE, CA, CB, CC, CD, CE, DA, DB, DC, DD, DE]) -> 
    [AB, AC, AD, AE, AA, BB, BC, BD, BE, BA, CB, CC, CD, CE, CA, DB, DC, DD, DE, DA].

x02([AA, AB, AC, AD, AE]) -> 
    [AC, AD, AE, AA, AB];
x02([AA, AB, AC, AD, AE, BA, BB, BC, BD, BE, CA, CB, CC, CD, CE, DA, DB, DC, DD, DE]) -> 
    [AC, AD, AE, AA, AB, BC, BD, BE, BA, BB, CC, CD, CE, CA, CB, DC, DD, DE, DA, DB].

x03([AA, AB, AC, AD, AE]) -> 
    [AD, AE, AA, AB, AC];
x03([AA, AB, AC, AD, AE, BA, BB, BC, BD, BE, CA, CB, CC, CD, CE, DA, DB, DC, DD, DE]) -> 
    [AD, AE, AA, AB, AC, BD, BE, BA, BB, BC, CD, CE, CA, CB, CC, DD, DE, DA, DB, DC].

x04([AA, AB, AC, AD, AE]) -> 
    [AE, AA, AB, AC, AD];
x04([AA, AB, AC, AD, AE, BA, BB, BC, BD, BE, CA, CB, CC, CD, CE, DA, DB, DC, DD, DE]) -> 
    [AE, AA, AB, AC, AD, BE, BA, BB, BC, BD, CE, CA, CB, CC, CD, DE, DA, DB, DC, DD].


y01([AA, AB, AC, AD, AE, BA, BB, BC, BD, BE, CA, CB, CC, CD, CE, DA, DB, DC, DD, DE]) -> 
    [BA, BB, BC, BD, BE, CA, CB, CC, CD, CE, DA, DB, DC, DD, DE, AA, AB, AC, AD, AE].

y02([AA, AB, AC, AD, AE, BA, BB, BC, BD, BE, CA, CB, CC, CD, CE, DA, DB, DC, DD, DE]) -> 
    [CA, CB, CC, CD, CE, DA, DB, DC, DD, DE, AA, AB, AC, AD, AE, BA, BB, BC, BD, BE].

y03([AA, AB, AC, AD, AE, BA, BB, BC, BD, BE, CA, CB, CC, CD, CE, DA, DB, DC, DD, DE]) -> 
    [DA, DB, DC, DD, DE, AA, AB, AC, AD, AE, BA, BB, BC, BD, BE, CA, CB, CC, CD, CE].


