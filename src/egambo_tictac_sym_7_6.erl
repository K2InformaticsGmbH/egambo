-module(egambo_tictac_sym_7_6).

-export([ide/1, ver/1, hor/1, pnt/1]).

-export([x01/1, x02/1, x03/1, x04/1, x05/1, x06/1]).

-export([y01/1, y02/1, y03/1, y04/1, y05/1]).

% generated in egambo_tictac_sym

ide(Board) -> Board.

ver([AA, AB, AC, AD, AE, AF, AG]) -> 
    [AG, AF, AE, AD, AC, AB, AA];

ver([AA, AB, AC, AD, AE, AF, AG, BA, BB, BC, BD, BE, BF, BG, CA, CB, CC, CD, CE, CF, CG, DA, DB, DC, DD, DE, DF, DG, EA, EB, EC, ED, EE, EF, EG, FA, FB, FC, FD, FE, FF, FG]) -> 
    [AG, AF, AE, AD, AC, AB, AA, BG, BF, BE, BD, BC, BB, BA, CG, CF, CE, CD, CC, CB, CA, DG, DF, DE, DD, DC, DB, DA, EG, EF, EE, ED, EC, EB, EA, FG, FF, FE, FD, FC, FB, FA].

hor([AA, AB, AC, AD, AE, AF, AG, BA, BB, BC, BD, BE, BF, BG, CA, CB, CC, CD, CE, CF, CG, DA, DB, DC, DD, DE, DF, DG, EA, EB, EC, ED, EE, EF, EG, FA, FB, FC, FD, FE, FF, FG]) -> 
    [FA, FB, FC, FD, FE, FF, FG, EA, EB, EC, ED, EE, EF, EG, DA, DB, DC, DD, DE, DF, DG, CA, CB, CC, CD, CE, CF, CG, BA, BB, BC, BD, BE, BF, BG, AA, AB, AC, AD, AE, AF, AG].

pnt([AA, AB, AC, AD, AE, AF, AG, BA, BB, BC, BD, BE, BF, BG, CA, CB, CC, CD, CE, CF, CG, DA, DB, DC, DD, DE, DF, DG, EA, EB, EC, ED, EE, EF, EG, FA, FB, FC, FD, FE, FF, FG]) -> 
    [FG, FF, FE, FD, FC, FB, FA, EG, EF, EE, ED, EC, EB, EA, DG, DF, DE, DD, DC, DB, DA, CG, CF, CE, CD, CC, CB, CA, BG, BF, BE, BD, BC, BB, BA, AG, AF, AE, AD, AC, AB, AA].


x01([AA, AB, AC, AD, AE, AF, AG]) -> 
    [AB, AC, AD, AE, AF, AG, AA];
x01([AA, AB, AC, AD, AE, AF, AG, BA, BB, BC, BD, BE, BF, BG, CA, CB, CC, CD, CE, CF, CG, DA, DB, DC, DD, DE, DF, DG, EA, EB, EC, ED, EE, EF, EG, FA, FB, FC, FD, FE, FF, FG]) -> 
    [AB, AC, AD, AE, AF, AG, AA, BB, BC, BD, BE, BF, BG, BA, CB, CC, CD, CE, CF, CG, CA, DB, DC, DD, DE, DF, DG, DA, EB, EC, ED, EE, EF, EG, EA, FB, FC, FD, FE, FF, FG, FA].

x02([AA, AB, AC, AD, AE, AF, AG]) -> 
    [AC, AD, AE, AF, AG, AA, AB];
x02([AA, AB, AC, AD, AE, AF, AG, BA, BB, BC, BD, BE, BF, BG, CA, CB, CC, CD, CE, CF, CG, DA, DB, DC, DD, DE, DF, DG, EA, EB, EC, ED, EE, EF, EG, FA, FB, FC, FD, FE, FF, FG]) -> 
    [AC, AD, AE, AF, AG, AA, AB, BC, BD, BE, BF, BG, BA, BB, CC, CD, CE, CF, CG, CA, CB, DC, DD, DE, DF, DG, DA, DB, EC, ED, EE, EF, EG, EA, EB, FC, FD, FE, FF, FG, FA, FB].

x03([AA, AB, AC, AD, AE, AF, AG]) -> 
    [AD, AE, AF, AG, AA, AB, AC];
x03([AA, AB, AC, AD, AE, AF, AG, BA, BB, BC, BD, BE, BF, BG, CA, CB, CC, CD, CE, CF, CG, DA, DB, DC, DD, DE, DF, DG, EA, EB, EC, ED, EE, EF, EG, FA, FB, FC, FD, FE, FF, FG]) -> 
    [AD, AE, AF, AG, AA, AB, AC, BD, BE, BF, BG, BA, BB, BC, CD, CE, CF, CG, CA, CB, CC, DD, DE, DF, DG, DA, DB, DC, ED, EE, EF, EG, EA, EB, EC, FD, FE, FF, FG, FA, FB, FC].

x04([AA, AB, AC, AD, AE, AF, AG]) -> 
    [AE, AF, AG, AA, AB, AC, AD];
x04([AA, AB, AC, AD, AE, AF, AG, BA, BB, BC, BD, BE, BF, BG, CA, CB, CC, CD, CE, CF, CG, DA, DB, DC, DD, DE, DF, DG, EA, EB, EC, ED, EE, EF, EG, FA, FB, FC, FD, FE, FF, FG]) -> 
    [AE, AF, AG, AA, AB, AC, AD, BE, BF, BG, BA, BB, BC, BD, CE, CF, CG, CA, CB, CC, CD, DE, DF, DG, DA, DB, DC, DD, EE, EF, EG, EA, EB, EC, ED, FE, FF, FG, FA, FB, FC, FD].

x05([AA, AB, AC, AD, AE, AF, AG]) -> 
    [AF, AG, AA, AB, AC, AD, AE];
x05([AA, AB, AC, AD, AE, AF, AG, BA, BB, BC, BD, BE, BF, BG, CA, CB, CC, CD, CE, CF, CG, DA, DB, DC, DD, DE, DF, DG, EA, EB, EC, ED, EE, EF, EG, FA, FB, FC, FD, FE, FF, FG]) -> 
    [AF, AG, AA, AB, AC, AD, AE, BF, BG, BA, BB, BC, BD, BE, CF, CG, CA, CB, CC, CD, CE, DF, DG, DA, DB, DC, DD, DE, EF, EG, EA, EB, EC, ED, EE, FF, FG, FA, FB, FC, FD, FE].

x06([AA, AB, AC, AD, AE, AF, AG]) -> 
    [AG, AA, AB, AC, AD, AE, AF];
x06([AA, AB, AC, AD, AE, AF, AG, BA, BB, BC, BD, BE, BF, BG, CA, CB, CC, CD, CE, CF, CG, DA, DB, DC, DD, DE, DF, DG, EA, EB, EC, ED, EE, EF, EG, FA, FB, FC, FD, FE, FF, FG]) -> 
    [AG, AA, AB, AC, AD, AE, AF, BG, BA, BB, BC, BD, BE, BF, CG, CA, CB, CC, CD, CE, CF, DG, DA, DB, DC, DD, DE, DF, EG, EA, EB, EC, ED, EE, EF, FG, FA, FB, FC, FD, FE, FF].


y01([AA, AB, AC, AD, AE, AF, AG, BA, BB, BC, BD, BE, BF, BG, CA, CB, CC, CD, CE, CF, CG, DA, DB, DC, DD, DE, DF, DG, EA, EB, EC, ED, EE, EF, EG, FA, FB, FC, FD, FE, FF, FG]) -> 
    [BA, BB, BC, BD, BE, BF, BG, CA, CB, CC, CD, CE, CF, CG, DA, DB, DC, DD, DE, DF, DG, EA, EB, EC, ED, EE, EF, EG, FA, FB, FC, FD, FE, FF, FG, AA, AB, AC, AD, AE, AF, AG].

y02([AA, AB, AC, AD, AE, AF, AG, BA, BB, BC, BD, BE, BF, BG, CA, CB, CC, CD, CE, CF, CG, DA, DB, DC, DD, DE, DF, DG, EA, EB, EC, ED, EE, EF, EG, FA, FB, FC, FD, FE, FF, FG]) -> 
    [CA, CB, CC, CD, CE, CF, CG, DA, DB, DC, DD, DE, DF, DG, EA, EB, EC, ED, EE, EF, EG, FA, FB, FC, FD, FE, FF, FG, AA, AB, AC, AD, AE, AF, AG, BA, BB, BC, BD, BE, BF, BG].

y03([AA, AB, AC, AD, AE, AF, AG, BA, BB, BC, BD, BE, BF, BG, CA, CB, CC, CD, CE, CF, CG, DA, DB, DC, DD, DE, DF, DG, EA, EB, EC, ED, EE, EF, EG, FA, FB, FC, FD, FE, FF, FG]) -> 
    [DA, DB, DC, DD, DE, DF, DG, EA, EB, EC, ED, EE, EF, EG, FA, FB, FC, FD, FE, FF, FG, AA, AB, AC, AD, AE, AF, AG, BA, BB, BC, BD, BE, BF, BG, CA, CB, CC, CD, CE, CF, CG].

y04([AA, AB, AC, AD, AE, AF, AG, BA, BB, BC, BD, BE, BF, BG, CA, CB, CC, CD, CE, CF, CG, DA, DB, DC, DD, DE, DF, DG, EA, EB, EC, ED, EE, EF, EG, FA, FB, FC, FD, FE, FF, FG]) -> 
    [EA, EB, EC, ED, EE, EF, EG, FA, FB, FC, FD, FE, FF, FG, AA, AB, AC, AD, AE, AF, AG, BA, BB, BC, BD, BE, BF, BG, CA, CB, CC, CD, CE, CF, CG, DA, DB, DC, DD, DE, DF, DG].

y05([AA, AB, AC, AD, AE, AF, AG, BA, BB, BC, BD, BE, BF, BG, CA, CB, CC, CD, CE, CF, CG, DA, DB, DC, DD, DE, DF, DG, EA, EB, EC, ED, EE, EF, EG, FA, FB, FC, FD, FE, FF, FG]) -> 
    [FA, FB, FC, FD, FE, FF, FG, AA, AB, AC, AD, AE, AF, AG, BA, BB, BC, BD, BE, BF, BG, CA, CB, CC, CD, CE, CF, CG, DA, DB, DC, DD, DE, DF, DG, EA, EB, EC, ED, EE, EF, EG].


