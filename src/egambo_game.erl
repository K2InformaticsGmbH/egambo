-module(egambo_game).

-include("egambo_game.hrl").

-behavior(gen_server).

-define(EG_MSG_TABLE_OPTS,  [{record_name, egGameMsg}
                            ,{type, ordered_set}
                            ,{purge_delay, 430000}      %% 430000 = 5 Days - 2000 sec
                            ]).

-define(MAX_BATCH_COUNT, 10000).

-define(BAD_BATCH_START_REQUEST, {error, <<"Bad batch start command or batch count exceeded">>}).
-define(NO_SUCH_GAME_TYPE, {error, <<"Game type does not exist">>}).
-define(NO_SUCH_GAME, {error, <<"Game does not exist">>}).
-define(UNIQUE_PLAYERS, {error, <<"You cannot play against yourself">>}).
-define(NOT_YOUR_GAME, {error, <<"This is not your game">>}).
-define(CANCEL_STATUS(__ST), {error, <<"You cannot cancel a game in status ", __ST/binary>>}).
-define(ACCEPT_MISMATCH, {error, <<"You cannot accept a challenge for another player">>}).
-define(ACCEPT_STATUS(__ST), {error, <<"You cannot accept a game in status ", __ST/binary>>}).
-define(BAD_COMMAND, {error, <<"Bad command or parameter format">>}).
-define(BAD_ACCOUNT, {error, <<"Bad account format">>}).

-define(DEFAULT_GAME_CATEGORIES, [ {egGameCategory,<<"tictac_challenge">>,<<"Tic Tac Challenge">>,<<"Two players alternate in placing their stones on a square board. A minimum number of consecutive stones in horizontal, vertical or diagonal direction wins. ">>}
                                 ]).
-define(DEFAULT_GAME_TYPES, [
  {egGameType,<<"connect_7_2jjo">>,<<"8x8:7 Connect 5 2 jokers 1 block">>,<<"tictac_challenge">>,egambo_tictac,2,#{aliases => "XO",gravity => true,height => 8,jokers => 2,obstacles => 1,periodic => false,run => 7,width => 8},undefined,2,<<"Connect 5 on 8x8 with jokers and block ">>}
, {egGameType,<<"connect_four">>,<<"7x6:4g Connect Four classic">>,<<"tictac_challenge">>,egambo_tictac,2,#{aliases => "XO",gravity => true,height => 6,jokers => 0,obstacles => 0,periodic => false,run => 4,width => 7},undefined,2,<<"Classic Connect Four (also Captain's mistress) ">>}
, {egGameType,<<"gomoku_13">>,<<"13x13:5 Gomoku small ">>,<<"tictac_challenge">>,egambo_tictac,2,#{aliases => "XO",gravity => false,height => 13,jokers => 0,obstacles => 0,periodic => false,run => 5,width => 13},undefined,2,<<"Small Board Gomoku on 13x13">>}
, {egGameType,<<"gomoku_15">>,<<"15x15:5 Gomoku medium ">>,<<"tictac_challenge">>,egambo_tictac,2,#{aliases => "XO",gravity => false,height => 15,jokers => 0,obstacles => 0,periodic => false,run => 5,width => 15},undefined,2,<<"Medium Board Gomoku on 15x15">>}
, {egGameType,<<"gomoku_15_jjoo">>,<<"15x15:5 Gomoku medium 2 jokers 2 obstacles">>,<<"tictac_challenge">>,egambo_tictac,2,#{aliases => "XO",gravity => false,height => 15,jokers => 2,obstacles => 2,periodic => false,run => 5,width => 15},undefined,2,<<"Medium Board Gomoku on 15x15 2 jokers to obstacles">>}
, {egGameType,<<"gomoku_19">>,<<"19x19:5 Gomoku large ">>,<<"tictac_challenge">>,egambo_tictac,2,#{aliases => "XO",gravity => false,height => 19,jokers => 0,obstacles => 0,periodic => false,run => 5,width => 19},undefined,2,<<"Large Board Gomoku on 18x18">>}
, {egGameType,<<"gomoku_8">>,<<"8x8:5 Gomoku tiny ">>,<<"tictac_challenge">>,egambo_tictac,2,#{aliases => "XO",gravity => false,height => 8,jokers => 0,obstacles => 0,periodic => false,run => 5,width => 8},undefined,2,<<"Tiny Board Gomoku on 8x8 ">>}
, {egGameType,<<"gomoku_8_p">>,<<"8x8:5 Gomoku small periodic ">>,<<"tictac_challenge">>,egambo_tictac,2,#{aliases => "XO",gravity => false,height => 8,jokers => 0,obstacles => 0,periodic => true,run => 5,width => 8},undefined,2,<<"Small Board Gomoku on 8x8 periodic">>}
, {egGameType,<<"tic_tac_toe">>,<<"3x3:3 Tic-Tac-Toe classic">>,<<"tictac_challenge">>,egambo_tictac,2,#{aliases => "XO",gravity => false,height => 3,jokers => 0,obstacles => 0,periodic => false,run => 3,width => 3},undefined,1,<<"Tic-Tac-Toe classic ">>}
, {egGameType,<<"tic_tac_toe_443">>,<<"4x4:3 Tic-Tac-Toe">>,<<"tictac_challenge">>,egambo_tictac,2,#{aliases => "XO",gravity => false,height => 4,jokers => 0,obstacles => 0,periodic => false,run => 3,width => 4},undefined,1,<<"Tic-Tac-Toe-443 classic ">>}
, {egGameType,<<"tic_tac_toe_443_oj">>,<<"4x4:3 Tic-Tac-Toe 1 obstacle 1 joker">>,<<"tictac_challenge">>,egambo_tictac,2,#{aliases => "XO",gravity => false,height => 4,jokers => 1,obstacles => 1,periodic => false,run => 3,width => 4},undefined,1,<<"Tic-Tac-Toe 4x4:3 with obstacle and joker">>}
, {egGameType,<<"tic_tac_toe_g">>,<<"3x3:3 Tic-Tac-Toe with gravity">>,<<"tictac_challenge">>,egambo_tictac,2,#{aliases => "XO",gravity => true,height => 3,jokers => 0,obstacles => 0,periodic => false,run => 3,width => 3},undefined,1,<<"Tic-Tac-Toe with Gravity ">>}
, {egGameType,<<"tic_tac_toe_p">>,<<"3x3:3 Tic-Tac-Toe periodic">>,<<"tictac_challenge">>,egambo_tictac,2,#{aliases => "XO",gravity => false,height => 3,jokers => 0,obstacles => 0,periodic => true,run => 3,width => 3},undefined,1,<<"Tic-Tac-Toe on a periodic Board">>}
]).

-define(DEFAULT_ACCOUNTS, [
  {ddAccount,0,<<"bot0">>,egambo_tictac_ann,[{scrypt,{<<58,37,63,119,99,72,15,176,129,198,51,111,57,75,39,108,115,32,240,124,125,78,88,192,158,90,201,226,44,187,133,119>>,<<114,104,245,152,103,199,134,8,178,201,226,224,8,20,249,200,124,66,148,136,249,59,245,209,138,158,181,66,138,205,7,7,106,136,30,150,216,105,38,32,87,74,240,42,101,1,150,56,78,96,67,9,215,136,94,165,226,231,127,237,80,158,141,231>>}}],<<"Bot0 default ML tic_tac_bot">>,undefined,undefined,{{2017,3,26},{20,54,40}},false}
, {ddAccount,1,<<"bot1">>,egambo_tictac_bot,[{scrypt,{<<58,37,63,119,99,72,15,176,129,198,51,111,57,75,39,108,115,32,240,124,125,78,88,192,158,90,201,226,44,187,133,119>>,<<114,104,245,152,103,199,134,8,178,201,226,224,8,20,249,200,124,66,148,136,249,59,245,209,138,158,181,66,138,205,7,7,106,136,30,150,216,105,38,32,87,74,240,42,101,1,150,56,78,96,67,9,215,136,94,165,226,231,127,237,80,158,141,231>>}}],<<"Bot1 simple tic_tac_bot">>,undefined,undefined,{{2017,3,26},{20,54,40}},false}
, {ddAccount,2,<<"bot2">>,egambo_tictac_bot,[{scrypt,{<<58,37,63,119,99,72,15,176,129,198,51,111,57,75,39,108,115,32,240,124,125,78,88,192,158,90,201,226,44,187,133,119>>,<<114,104,245,152,103,199,134,8,178,201,226,224,8,20,249,200,124,66,148,136,249,59,245,209,138,158,181,66,138,205,7,7,106,136,30,150,216,105,38,32,87,74,240,42,101,1,150,56,78,96,67,9,215,136,94,165,226,231,127,237,80,158,141,231>>}}],<<"Bot2 simple tic_tac_bot">>,undefined,undefined,{{2017,3,26},{20,54,40}},false}
, {ddAccount,3,<<"player3">>,user,[{scrypt,{<<58,37,63,119,99,72,15,176,129,198,51,111,57,75,39,108,115,32,240,124,125,78,88,192,158,90,201,226,44,187,133,119>>,<<114,104,245,152,103,199,134,8,178,201,226,224,8,20,249,200,124,66,148,136,249,59,245,209,138,158,181,66,138,205,7,7,106,136,30,150,216,105,38,32,87,74,240,42,101,1,150,56,78,96,67,9,215,136,94,165,226,231,127,237,80,158,141,231>>}}],<<"Player3">>,undefined,undefined,{{2017,3,26},{20,54,40}},false}
, {ddAccount,4,<<"player4">>,user,[{scrypt,{<<58,37,63,119,99,72,15,176,129,198,51,111,57,75,39,108,115,32,240,124,125,78,88,192,158,90,201,226,44,187,133,119>>,<<114,104,245,152,103,199,134,8,178,201,226,224,8,20,249,200,124,66,148,136,249,59,245,209,138,158,181,66,138,205,7,7,106,136,30,150,216,105,38,32,87,74,240,42,101,1,150,56,78,96,67,9,215,136,94,165,226,231,127,237,80,158,141,231>>}}],<<"Player4">>,undefined,undefined,{{2017,3,26},{20,54,40}},false}
, {ddAccount,5,<<"c4bot">>,egambo_connect_four_bot,[{scrypt,{<<58,37,63,119,99,72,15,176,129,198,51,111,57,75,39,108,115,32,240,124,125,78,88,192,158,90,201,226,44,187,133,119>>,<<114,104,245,152,103,199,134,8,178,201,226,224,8,20,249,200,124,66,148,136,249,59,245,209,138,158,181,66,138,205,7,7,106,136,30,150,216,105,38,32,87,74,240,42,101,1,150,56,78,96,67,9,215,136,94,165,226,231,127,237,80,158,141,231>>}}],<<"ANN connect four bot">>,undefined,undefined,{{2017,3,26},{20,54,40}},false}
]).

-define(DEFAULT_ROLES, [
  {ddRole,system,[egambo,dderl],[manage_system,manage_accounts,manage_system_tables,manage_user_tables,{dderl,con,local,use}],[]}
, {ddRole,dderl,[],[{dderl,restart},{dderl,conn,local,use},{dderl,conn,{owner,system},use},{dderl,conn,manage}],[]}
, {ddRole,egambo,[],[{eval_mfa,egambo_game,create}
                    ,{eval_mfa,egambo_game,start}
                    ,{eval_mfa,egambo_game,cancel}
                    ,{eval_mfa,egambo_game,play}
                    ,{eval_mfa,egambo_game,result}
                    ,{eval_mfa,egambo_game,resume}
                    ,{eval_mfa,egambo_game,moves}
                    ,{eval_mfa,egambo_tictac,sample}
                    ,{eval_mfa,egambo_tictac,samples}
                    ,{eval_mfa,egambo_tictac,history}
                    ],[]}
, {ddRole,ann,[],[{eval_mfa,egambo_tictac_ann,state}
                 ,{eval_mfa,egambo_tictac_ann,start}
                 ,{eval_mfa,egambo_tictac_ann,stop}
                 ,{eval_mfa,egambo_tictac_ann,resume}
                 ,{eval_mfa,egambo_tictac_ann,save}
                 ,{eval_mfa,egambo_tictac_ann,start_learning}
                 ,{eval_mfa,egambo_tictac_ann,start_playing}
                 ,{eval_mfa,egambo_tictac_ann,ann_sample}
                 ,{eval_mfa,egambo_tictac_ann,train}
                 ,{eval_mfa,egambo_tictac_ann,test}
                 ],[]}
, {ddRole,0,[],[],[]}
, {ddRole,1,[],[],[]}
, {ddRole,2,[],[],[]}
, {ddRole,3,[system],[],[]}
, {ddRole,4,[system],[],[]}
, {ddRole,5,[],[],[]}
]).

-define(DEFAULT_VIEWS, [
  {ddView,113323941,undefined,system,<<"ddAccount">>,98509861,{viewstate,[{<<"width">>,1193},{<<"height">>,188.375},{<<"y">>,25.375},{<<"x">>,418},{<<"plane_to_show">>,0},{<<"plane_specs">>,null},{<<"start_btn">>,<<">">>}],[[{<<"name">>,<<"id_1">>},{<<"width">>,57.4063},{<<"hidden">>,false}],[{<<"name">>,<<"name_2">>},{<<"width">>,85.4063},{<<"hidden">>,false}],[{<<"name">>,<<"type_3">>},{<<"width">>,160.375},{<<"hidden">>,false}],[{<<"name">>,<<"credentials_4">>},{<<"width">>,184},{<<"hidden">>,false}],[{<<"name">>,<<"fullName_5">>},{<<"width">>,230.391},{<<"hidden">>,false}],[{<<"name">>,<<"lastLoginTime_6">>},{<<"width">>,103.0156},{<<"hidden">>,false}],[{<<"name">>,<<"lastFailureTime_7">>},{<<"width">>,115.0156},{<<"hidden">>,false}],[{<<"name">>,<<"lastPasswordChangeTime_8">>},{<<"width">>,159.6434},{<<"hidden">>,false}],[{<<"name">>,<<"locked_9">>},{<<"width">>,61.0156},{<<"hidden">>,false}]]}}
, {ddView,18518971,undefined,system,<<"ddConfig">>,90158797,{viewstate,[{<<"width">>,1248},{<<"height">>,328.375},{<<"y">>,191},{<<"x">>,262},{<<"plane_to_show">>,0},{<<"plane_specs">>,[[{<<"script">>,<<>>}]]},{<<"start_btn">>,<<">">>}],[[{<<"name">>,<<"hkl_1">>},{<<"width">>,386.453},{<<"hidden">>,false}],[{<<"name">>,<<"val_2">>},{<<"width">>,322},{<<"hidden">>,false}],[{<<"name">>,<<"owner_3">>},{<<"width">>,159.6434},{<<"hidden">>,false}],[{<<"name">>,<<"remark_4">>},{<<"width">>,546.1092},{<<"hidden">>,false}]]}}
, {ddView,34249292,undefined,system,<<"ddRole">>,98106028,{viewstate,[{<<"width">>,1015.77},{<<"height">>,275.375},{<<"y">>,83},{<<"x">>,119},{<<"plane_to_show">>,0},{<<"plane_specs">>,[[{<<"script">>,<<>>}]]},{<<"start_btn">>,<<">">>}],[[{<<"name">>,<<"id_1">>},{<<"width">>,50.42184},{<<"hidden">>,false}],[{<<"name">>,<<"\"roles\"_2">>},{<<"width">>,67.0156},{<<"hidden">>,false}],[{<<"name">>,<<"permissions_3">>},{<<"width">>,781.3316000000001},{<<"hidden">>,false}],[{<<"name">>,<<"quotas_4">>},{<<"width">>,61.0156},{<<"hidden">>,false}]]}}
, {ddView,37794155,undefined,system,<<"egambo.create(GameType,Cnt,OpponentAcc)">>,27433988,{viewstate,[{<<"plane_specs">>,[[{<<"script">>,<<>>}]]},{<<"plane_to_show">>,0}],[]}}
, {ddView,64754767,undefined,system,<<"egambo.create(GameType,Cnt,OpponentAcc,OwnerAcc)">>,27253828,{viewstate,[{<<"width">>,1053},{<<"height">>,110.375},{<<"y">>,0},{<<"x">>,0},{<<"plane_to_show">>,0},{<<"plane_specs">>,[[{<<"script">>,<<>>}]]},{<<"start_btn">>,<<">">>}],[[{<<"name">>,<<"GameIDs_1">>},{<<"width">>,1000},{<<"hidden">>,false}]]}}
, {ddView,102361309,undefined,system,<<"egambo.egGame">>,72273526,{viewstate,[{<<"width">>,1310},{<<"height">>,525.375},{<<"y">>,22},{<<"x">>,0},{<<"plane_to_show">>,0},{<<"plane_specs">>,[[{<<"script">>,<<>>}]]},{<<"start_btn">>,<<">">>}],[[{<<"name">>,<<"gid_1">>},{<<"width">>,159.6434},{<<"hidden">>,false}],[{<<"name">>,<<"tid_2">>},{<<"width">>,126.02184000000001},{<<"hidden">>,false}],[{<<"name">>,<<"players_3">>},{<<"width">>,67.0156},{<<"hidden">>,false}],[{<<"name">>,<<"bots_4">>},{<<"width">>,310.86580000000004},{<<"hidden">>,false}],[{<<"name">>,<<"ctime_5">>},{<<"width">>,218.4434},{<<"hidden">>,false}],[{<<"name">>,<<"ialiases_6">>},{<<"width">>,73.0156},{<<"hidden">>,false}],[{<<"name">>,<<"imovers_7">>},{<<"width">>,67.0156},{<<"hidden">>,false}],[{<<"name">>,<<"space_8">>},{<<"width">>,55.0156},{<<"hidden">>,false}],[{<<"name">>,<<"stime_9">>},{<<"width">>,218.4434},{<<"hidden">>,false}],[{<<"name">>,<<"etime_10">>},{<<"width">>,218.4434},{<<"hidden">>,false}],[{<<"name">>,<<"status_11">>},{<<"width">>,67.22184},{<<"hidden">>,false}],[{<<"name">>,<<"board_12">>},{<<"width">>,428.46580000000006},{<<"hidden">>,false}],[{<<"name">>,<<"nmovers_13">>},{<<"width">>,67.0156},{<<"hidden">>,false}],[{<<"name">>,<<"naliases_14">>},{<<"width">>,73.0156},{<<"hidden">>,false}],[{<<"name">>,<<"nscores_15">>},{<<"width">>,67.0156},{<<"hidden">>,false}],[{<<"name">>,<<"moves_16">>},{<<"width">>,1000},{<<"hidden">>,false}]]}}
, {ddView,66871739,undefined,system,<<"egambo.egGame.Playing">>,91864289,{viewstate,[{<<"width">>,1010},{<<"height">>,273},{<<"y">>,89},{<<"x">>,137},{<<"plane_to_show">>,0},{<<"plane_specs">>,[[{<<"script">>,<<>>}]]},{<<"start_btn">>,<<">">>}],[[{<<"name">>,<<"gid_1">>},{<<"width">>,141.641},{<<"hidden">>,false}],[{<<"name">>,<<"tid_2">>},{<<"width">>,140.39100000000002},{<<"hidden">>,false}],[{<<"name">>,<<"players_3">>},{<<"width">>,66},{<<"hidden">>,false}],[{<<"name">>,<<"bots_4">>},{<<"width">>,150.438},{<<"hidden">>,false}],[{<<"name">>,<<"ctime_5">>},{<<"width">>,115.438},{<<"hidden">>,false}],[{<<"name">>,<<"ialiases_6">>},{<<"width">>,70},{<<"hidden">>,false}],[{<<"name">>,<<"imovers_7">>},{<<"width">>,68},{<<"hidden">>,false}],[{<<"name">>,<<"space_8">>},{<<"width">>,113},{<<"hidden">>,false}],[{<<"name">>,<<"stime_9">>},{<<"width">>,80.4375},{<<"hidden">>,false}],[{<<"name">>,<<"etime_10">>},{<<"width">>,75.6094},{<<"hidden">>,false}],[{<<"name">>,<<"status_11">>},{<<"width">>,58.8125},{<<"hidden">>,false}],[{<<"name">>,<<"board_12">>},{<<"width">>,232},{<<"hidden">>,false}],[{<<"name">>,<<"nmovers_13">>},{<<"width">>,65},{<<"hidden">>,false}],[{<<"name">>,<<"naliases_14">>},{<<"width">>,78},{<<"hidden">>,false}],[{<<"name">>,<<"nscores_15">>},{<<"width">>,61},{<<"hidden">>,false}],[{<<"name">>,<<"moves_16">>},{<<"width">>,126},{<<"hidden">>,false}]]}}
, {ddView,78555294,undefined,system,<<"egambo.egGame.Recent">>,75649981,{viewstate,[{<<"width">>,1070},{<<"height">>,209.375},{<<"y">>,164.625},{<<"x">>,157},{<<"plane_to_show">>,0},{<<"plane_specs">>,[[{<<"script">>,<<>>}]]},{<<"start_btn">>,<<">">>}],[[{<<"name">>,<<"gid_1">>},{<<"width">>,141.641},{<<"hidden">>,false}],[{<<"name">>,<<"tid_2">>},{<<"width">>,104.375},{<<"hidden">>,false}],[{<<"name">>,<<"imovers_3">>},{<<"width">>,68},{<<"hidden">>,false}],[{<<"name">>,<<"status_4">>},{<<"width">>,65.8125},{<<"hidden">>,false}],[{<<"name">>,<<"board_5">>},{<<"width">>,237},{<<"hidden">>,false}],[{<<"name">>,<<"nmovers_6">>},{<<"width">>,65},{<<"hidden">>,false}],[{<<"name">>,<<"naliases_7">>},{<<"width">>,78},{<<"hidden">>,false}],[{<<"name">>,<<"nscores_8">>},{<<"width">>,61},{<<"hidden">>,false}],[{<<"name">>,<<"moves_9">>},{<<"width">>,240},{<<"hidden">>,false}]]}}
, {ddView,40881952,undefined,system,<<"egambo.egGame.Sampler(GameType)">>,8606876,{viewstate,[{<<"width">>,1326},{<<"height">>,366.375},{<<"y">>,209},{<<"x">>,0},{<<"plane_to_show">>,0},{<<"plane_specs">>,[[{<<"script">>,<<>>}]]},{<<"start_btn">>,<<">">>}],[[{<<"name">>,<<"gid_1">>},{<<"width">>,159.641},{<<"hidden">>,false}],[{<<"name">>,<<"tid_2">>},{<<"width">>,126.016},{<<"hidden">>,false}],[{<<"name">>,<<"imovers_3">>},{<<"width">>,67},{<<"hidden">>,false}],[{<<"name">>,<<"status_4">>},{<<"width">>,67.2188},{<<"hidden">>,false}],[{<<"name">>,<<"board_5">>},{<<"width">>,92.4063},{<<"hidden">>,false}],[{<<"name">>,<<"nmovers_6">>},{<<"width">>,67},{<<"hidden">>,false}],[{<<"name">>,<<"naliases_7">>},{<<"width">>,73},{<<"hidden">>,false}],[{<<"name">>,<<"nscores_8">>},{<<"width">>,67},{<<"hidden">>,false}],[{<<"name">>,<<"sample_9">>},{<<"width">>,244.438},{<<"hidden">>,false}],[{<<"name">>,<<"moves_10">>},{<<"width">>,613.3091999999999},{<<"hidden">>,false}]]}}
, {ddView,119584524,undefined,system,<<"egambo.egGame.Unfinished">>,12106255,{viewstate,[{<<"width">>,1197},{<<"height">>,121.375},{<<"y">>,441.625},{<<"x">>,62},{<<"plane_to_show">>,0},{<<"plane_specs">>,[[{<<"script">>,<<>>}]]},{<<"start_btn">>,<<">">>}],[[{<<"name">>,<<"gid_1">>},{<<"width">>,141.641},{<<"hidden">>,false}],[{<<"name">>,<<"tid_2">>},{<<"width">>,140.391},{<<"hidden">>,false}],[{<<"name">>,<<"bots_3">>},{<<"width">>,260.438},{<<"hidden">>,false}],[{<<"name">>,<<"status_4">>},{<<"width">>,58.8125},{<<"hidden">>,false}],[{<<"name">>,<<"board_5">>},{<<"width">>,232},{<<"hidden">>,false}],[{<<"name">>,<<"nmovers_6">>},{<<"width">>,65},{<<"hidden">>,false}],[{<<"name">>,<<"naliases_7">>},{<<"width">>,78},{<<"hidden">>,false}],[{<<"name">>,<<"moves_8">>},{<<"width">>,126},{<<"hidden">>,false}]]}}
, {ddView,115105667,undefined,system,<<"egambo.egGame.Width13">>,25460815,{viewstate,[{<<"width">>,995.516},{<<"height">>,350.375},{<<"y">>,90},{<<"x">>,172.46875},{<<"plane_to_show">>,0},{<<"plane_specs">>,[[{<<"script">>,<<>>}]]},{<<"start_btn">>,<<">">>}],[[{<<"name">>,<<"gid_1">>},{<<"width">>,159.6434},{<<"hidden">>,false}],[{<<"name">>,<<"tid_2">>},{<<"width">>,117.62184000000002},{<<"hidden">>,false}],[{<<"name">>,<<"status_3">>},{<<"width">>,61.0156},{<<"hidden">>,false}],[{<<"name">>,<<"nmovers_4">>},{<<"width">>,67.0156},{<<"hidden">>,false}],[{<<"name">>,<<"naliases_5">>},{<<"width">>,73.0156},{<<"hidden">>,false}],[{<<"name">>,<<"row_6">>},{<<"width">>,43.0156},{<<"hidden">>,false}],[{<<"name">>,<<"a_7">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"b_8">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"c_9">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"d_10">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"e_11">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"f_12">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"g_13">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"h_14">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"i_15">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"j_16">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"k_17">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"l_18">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"m_19">>},{<<"width">>,31.01563},{<<"hidden">>,false}]]}}
, {ddView,114990539,undefined,system,<<"egambo.egGame.Width15">>,64086186,{viewstate,[{<<"width">>,1060.55},{<<"height">>,385.375},{<<"y">>,55},{<<"x">>,197},{<<"plane_to_show">>,0},{<<"plane_specs">>,[[{<<"script">>,<<>>}]]},{<<"start_btn">>,<<">">>}],[[{<<"name">>,<<"gid_1">>},{<<"width">>,159.6434},{<<"hidden">>,false}],[{<<"name">>,<<"tid_2">>},{<<"width">>,117.62184000000002},{<<"hidden">>,false}],[{<<"name">>,<<"status_3">>},{<<"width">>,61.0156},{<<"hidden">>,false}],[{<<"name">>,<<"nmovers_4">>},{<<"width">>,67.0156},{<<"hidden">>,false}],[{<<"name">>,<<"naliases_5">>},{<<"width">>,73.0156},{<<"hidden">>,false}],[{<<"name">>,<<"row_6">>},{<<"width">>,43.0156},{<<"hidden">>,false}],[{<<"name">>,<<"a_7">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"b_8">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"c_9">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"d_10">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"e_11">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"f_12">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"g_13">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"h_14">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"i_15">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"j_16">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"k_17">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"l_18">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"m_19">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"n_20">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"o_21">>},{<<"width">>,31.01563},{<<"hidden">>,false}]]}}
, {ddView,48584326,undefined,system,<<"egambo.egGame.Width19">>,66113065,{viewstate,[{<<"width">>,1164.52},{<<"height">>,464.375},{<<"y">>,26},{<<"x">>,125.46875},{<<"plane_to_show">>,0},{<<"plane_specs">>,[[{<<"script">>,<<>>}]]},{<<"start_btn">>,<<">">>}],[[{<<"name">>,<<"gid_1">>},{<<"width">>,159.6434},{<<"hidden">>,false}],[{<<"name">>,<<"tid_2">>},{<<"width">>,75.62183999999999},{<<"hidden">>,false}],[{<<"name">>,<<"status_3">>},{<<"width">>,61.0156},{<<"hidden">>,false}],[{<<"name">>,<<"nmovers_4">>},{<<"width">>,67.0156},{<<"hidden">>,false}],[{<<"name">>,<<"naliases_5">>},{<<"width">>,73.0156},{<<"hidden">>,false}],[{<<"name">>,<<"row_6">>},{<<"width">>,43.0156},{<<"hidden">>,false}],[{<<"name">>,<<"a_7">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"b_8">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"c_9">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"d_10">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"e_11">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"f_12">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"g_13">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"h_14">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"i_15">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"j_16">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"k_17">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"l_18">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"m_19">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"n_20">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"o_21">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"p_22">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"p_23">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"q_24">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"r_25">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"s_26">>},{<<"width">>,31.01563},{<<"hidden">>,false}]]}}
, {ddView,127200102,undefined,system,<<"egambo.egGame.Width3">>,91783774,{viewstate,[{<<"width">>,681.578},{<<"height">>,150.375},{<<"y">>,241},{<<"x">>,156},{<<"plane_to_show">>,0},{<<"plane_specs">>,[[{<<"script">>,<<>>}]]},{<<"start_btn">>,<<">">>}],[[{<<"name">>,<<"gid_1">>},{<<"width">>,151.2434},{<<"hidden">>,false}],[{<<"name">>,<<"tid_2">>},{<<"width">>,126.02184000000001},{<<"hidden">>,false}],[{<<"name">>,<<"status_3">>},{<<"width">>,67.22184},{<<"hidden">>,false}],[{<<"name">>,<<"nmovers_4">>},{<<"width">>,67.0156},{<<"hidden">>,false}],[{<<"name">>,<<"naliases_5">>},{<<"width">>,73.0156},{<<"hidden">>,false}],[{<<"name">>,<<"row_6">>},{<<"width">>,43.0156},{<<"hidden">>,false}],[{<<"name">>,<<"a_7">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"b_8">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"c_9">>},{<<"width">>,31.01563},{<<"hidden">>,false}]]}}
, {ddView,113113335,undefined,system,<<"egambo.egGame.Width4">>,65884760,{viewstate,[{<<"width">>,713.594},{<<"height">>,170.375},{<<"y">>,59},{<<"x">>,464},{<<"plane_to_show">>,0},{<<"plane_specs">>,[[{<<"script">>,<<>>}]]},{<<"start_btn">>,<<">">>}],[[{<<"name">>,<<"gid_1">>},{<<"width">>,151.2434},{<<"hidden">>,false}],[{<<"name">>,<<"tid_2">>},{<<"width">>,126.02184000000001},{<<"hidden">>,false}],[{<<"name">>,<<"status_3">>},{<<"width">>,67.22184},{<<"hidden">>,false}],[{<<"name">>,<<"nmovers_4">>},{<<"width">>,67.0156},{<<"hidden">>,false}],[{<<"name">>,<<"naliases_5">>},{<<"width">>,73.0156},{<<"hidden">>,false}],[{<<"name">>,<<"row_6">>},{<<"width">>,43.0156},{<<"hidden">>,false}],[{<<"name">>,<<"a_7">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"b_8">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"c_9">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"d_10">>},{<<"width">>,31.01563},{<<"hidden">>,false}]]}}
, {ddView,70708171,undefined,system,<<"egambo.egGame.Width5">>,79512648,{viewstate,[{<<"width">>,739.391},{<<"height">>,190.375},{<<"y">>,79},{<<"x">>,363},{<<"plane_to_show">>,0},{<<"plane_specs">>,[[{<<"script">>,<<>>}]]},{<<"start_btn">>,<<">">>}],[[{<<"name">>,<<"gid_1">>},{<<"width">>,159.6434},{<<"hidden">>,false}],[{<<"name">>,<<"tid_2">>},{<<"width">>,117.62184000000002},{<<"hidden">>,false}],[{<<"name">>,<<"status_3">>},{<<"width">>,61.0156},{<<"hidden">>,false}],[{<<"name">>,<<"nmovers_4">>},{<<"width">>,67.0156},{<<"hidden">>,false}],[{<<"name">>,<<"naliases_5">>},{<<"width">>,73.0156},{<<"hidden">>,false}],[{<<"name">>,<<"row_6">>},{<<"width">>,43.0156},{<<"hidden">>,false}],[{<<"name">>,<<"a_7">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"b_8">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"c_9">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"d_10">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"e_11">>},{<<"width">>,31.01563},{<<"hidden">>,false}]]}}
, {ddView,20337143,undefined,system,<<"egambo.egGame.Width6">>,93207148,{viewstate,[{<<"width">>,771.406},{<<"height">>,210.375},{<<"y">>,79},{<<"x">>,363},{<<"plane_to_show">>,0},{<<"plane_specs">>,[[{<<"script">>,<<>>}]]},{<<"start_btn">>,<<">">>}],[[{<<"name">>,<<"gid_1">>},{<<"width">>,159.6434},{<<"hidden">>,false}],[{<<"name">>,<<"tid_2">>},{<<"width">>,117.62184000000002},{<<"hidden">>,false}],[{<<"name">>,<<"status_3">>},{<<"width">>,61.0156},{<<"hidden">>,false}],[{<<"name">>,<<"nmovers_4">>},{<<"width">>,67.0156},{<<"hidden">>,false}],[{<<"name">>,<<"naliases_5">>},{<<"width">>,73.0156},{<<"hidden">>,false}],[{<<"name">>,<<"row_6">>},{<<"width">>,43.0156},{<<"hidden">>,false}],[{<<"name">>,<<"a_7">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"b_8">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"c_9">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"d_10">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"e_11">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"f_12">>},{<<"width">>,31.01563},{<<"hidden">>,false}]]}}
, {ddView,26126179,undefined,system,<<"egambo.egGame.Width7">>,23833684,{viewstate,[{<<"width">>,803.422},{<<"height">>,230.375},{<<"y">>,79},{<<"x">>,363},{<<"plane_to_show">>,0},{<<"plane_specs">>,[[{<<"script">>,<<>>}]]},{<<"start_btn">>,<<">">>}],[[{<<"name">>,<<"gid_1">>},{<<"width">>,159.6434},{<<"hidden">>,false}],[{<<"name">>,<<"tid_2">>},{<<"width">>,117.62184000000002},{<<"hidden">>,false}],[{<<"name">>,<<"status_3">>},{<<"width">>,61.0156},{<<"hidden">>,false}],[{<<"name">>,<<"nmovers_4">>},{<<"width">>,67.0156},{<<"hidden">>,false}],[{<<"name">>,<<"naliases_5">>},{<<"width">>,73.0156},{<<"hidden">>,false}],[{<<"name">>,<<"row_6">>},{<<"width">>,43.0156},{<<"hidden">>,false}],[{<<"name">>,<<"a_7">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"b_8">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"c_9">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"d_10">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"e_11">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"f_12">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"g_13">>},{<<"width">>,31.01563},{<<"hidden">>,false}]]}}
, {ddView,73050346,undefined,system,<<"egambo.egGame.Width8">>,85560651,{viewstate,[{<<"width">>,835.438},{<<"height">>,250.375},{<<"y">>,79},{<<"x">>,363},{<<"plane_to_show">>,0},{<<"plane_specs">>,[[{<<"script">>,<<>>}]]},{<<"start_btn">>,<<">">>}],[[{<<"name">>,<<"gid_1">>},{<<"width">>,159.6434},{<<"hidden">>,false}],[{<<"name">>,<<"tid_2">>},{<<"width">>,117.62184000000002},{<<"hidden">>,false}],[{<<"name">>,<<"status_3">>},{<<"width">>,61.0156},{<<"hidden">>,false}],[{<<"name">>,<<"nmovers_4">>},{<<"width">>,67.0156},{<<"hidden">>,false}],[{<<"name">>,<<"naliases_5">>},{<<"width">>,73.0156},{<<"hidden">>,false}],[{<<"name">>,<<"row_6">>},{<<"width">>,43.0156},{<<"hidden">>,false}],[{<<"name">>,<<"a_7">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"b_8">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"c_9">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"d_10">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"e_11">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"f_12">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"g_13">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"h_14">>},{<<"width">>,31.01563},{<<"hidden">>,false}]]}}
, {ddView,106412091,undefined,system,<<"egambo.egGame.Width9">>,102692664,{viewstate,[{<<"width">>,867.453},{<<"height">>,270.375},{<<"y">>,79},{<<"x">>,363},{<<"plane_to_show">>,0},{<<"plane_specs">>,[[{<<"script">>,<<>>}]]},{<<"start_btn">>,<<">">>}],[[{<<"name">>,<<"gid_1">>},{<<"width">>,159.6434},{<<"hidden">>,false}],[{<<"name">>,<<"tid_2">>},{<<"width">>,117.62184000000002},{<<"hidden">>,false}],[{<<"name">>,<<"status_3">>},{<<"width">>,61.0156},{<<"hidden">>,false}],[{<<"name">>,<<"nmovers_4">>},{<<"width">>,67.0156},{<<"hidden">>,false}],[{<<"name">>,<<"naliases_5">>},{<<"width">>,73.0156},{<<"hidden">>,false}],[{<<"name">>,<<"row_6">>},{<<"width">>,43.0156},{<<"hidden">>,false}],[{<<"name">>,<<"a_7">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"b_8">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"c_9">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"d_10">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"e_11">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"f_12">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"g_13">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"h_14">>},{<<"width">>,31.01563},{<<"hidden">>,false}],[{<<"name">>,<<"i_15">>},{<<"width">>,31.01563},{<<"hidden">>,false}]]}}
, {ddView,131789359,undefined,system,<<"egambo.egGameCategory">>,12450316,{viewstate,[{<<"width">>,690.016},{<<"height">>,112.375},{<<"y">>,124.375},{<<"x">>,556.375},{<<"plane_to_show">>,0},{<<"plane_specs">>,null},{<<"start_btn">>,<<">">>}],[[{<<"name">>,<<"cid_1">>},{<<"width">>,126},{<<"hidden">>,false}],[{<<"name">>,<<"cname_2">>},{<<"width">>,135},{<<"hidden">>,false}],[{<<"name">>,<<"info_3">>},{<<"width">>,376},{<<"hidden">>,false}]]}}
, {ddView,71140357,undefined,system,<<"egambo.egGameMsg">>,44230220,{viewstate,[{<<"width">>,1326.05},{<<"height">>,284.375},{<<"y">>,424.375},{<<"x">>,65.375},{<<"plane_to_show">>,0},{<<"plane_specs">>,null},{<<"start_btn">>,<<">">>}],[[{<<"name">>,<<"time_1">>},{<<"width">>,190},{<<"hidden">>,false}],[{<<"name">>,<<"gid_2">>},{<<"width">>,138},{<<"hidden">>,false}],[{<<"name">>,<<"msgtype_3">>},{<<"width">>,67},{<<"hidden">>,false}],[{<<"name">>,<<"message_4">>},{<<"width">>,852},{<<"hidden">>,false}]]}}
, {ddView,68768217,undefined,system,<<"egambo.egGameMsg.Recent">>,119553388,{viewstate,[{<<"width">>,1298.63},{<<"height">>,304.375},{<<"y">>,144.625},{<<"x">>,24.375},{<<"plane_to_show">>,0},{<<"plane_specs">>,[[{<<"script">>,<<>>}]]},{<<"start_btn">>,<<">">>}],[[{<<"name">>,<<"time_1">>},{<<"width">>,190},{<<"hidden">>,false}],[{<<"name">>,<<"gid_2">>},{<<"width">>,138},{<<"hidden">>,false}],[{<<"name">>,<<"msgtype_3">>},{<<"width">>,67},{<<"hidden">>,false}],[{<<"name">>,<<"message_4">>},{<<"width">>,852},{<<"hidden">>,false}]]}}
, {ddView,69308561,undefined,system,<<"egambo.egGameType">>,33484121,{viewstate,[{<<"width">>,1467.13},{<<"height">>,203.375},{<<"y">>,256},{<<"x">>,12},{<<"plane_to_show">>,0},{<<"plane_specs">>,[[{<<"script">>,<<>>}]]},{<<"start_btn">>,<<">">>}],[[{<<"name">>,<<"tid_1">>},{<<"width">>,167},{<<"hidden">>,false}],[{<<"name">>,<<"tname_2">>},{<<"width">>,218},{<<"hidden">>,false}],[{<<"name">>,<<"cid_3">>},{<<"width">>,136},{<<"hidden">>,false}],[{<<"name">>,<<"engine_4">>},{<<"width">>,103},{<<"hidden">>,false}],[{<<"name">>,<<"players_5">>},{<<"width">>,63},{<<"hidden">>,false}],[{<<"name">>,<<"params_6">>},{<<"width">>,293},{<<"hidden">>,false}],[{<<"name">>,<<"setup_7">>},{<<"width">>,104},{<<"hidden">>,false}],[{<<"name">>,<<"level_8">>},{<<"width">>,55.0156},{<<"hidden">>,false}],[{<<"name">>,<<"info_9">>},{<<"width">>,277.0002},{<<"hidden">>,false}]]}}
, {ddView,105248123,undefined,system,<<"egambo.play(GameId,CellAtom)">>,11410941,{viewstate,[{<<"width">>,399},{<<"height">>,90.375},{<<"y">>,0},{<<"x">>,0},{<<"plane_to_show">>,0},{<<"plane_specs">>,[[{<<"script">>,<<>>}]]},{<<"start_btn">>,<<">">>}],[[{<<"name">>,<<"Result_1">>},{<<"width">>,61.0156},{<<"hidden">>,false}]]}}
, {ddView,70351660,undefined,system,<<"egambo.play(GameId,CellInt)">>,125286160,{viewstate,[{<<"plane_specs">>,[[{<<"script">>,<<>>}]]},{<<"plane_to_show">>,0}],[]}}
, {ddView,48423064,undefined,system,<<"egambo.play_all(GameId,CellAtom)">>,4333626,{viewstate,[{<<"width">>,604},{<<"height">>,102.375},{<<"y">>,0},{<<"x">>,0},{<<"plane_to_show">>,0},{<<"plane_specs">>,[[{<<"script">>,<<>>}]]},{<<"start_btn">>,<<">">>}],[[{<<"name">>,<<"Result_1">>},{<<"width">>,510},{<<"hidden">>,false}]]}}
, {ddView,117581071,undefined,system,<<"egambo.play_all(GameId,CellInt)">>,23246841,{viewstate,[{<<"plane_specs">>,[[{<<"script">>,<<>>}]]},{<<"plane_to_show">>,0}],[]}}
, {ddView,134172068,undefined,system,<<"egambo.result(GameId)">>,70332437,{viewstate,[{<<"width">>,1053},{<<"height">>,110.375},{<<"y">>,200},{<<"x">>,705},{<<"plane_to_show">>,0},{<<"plane_specs">>,[[{<<"script">>,<<>>}]]},{<<"start_btn">>,<<">">>}],[[{<<"name">>,<<"Result_1">>},{<<"width">>,1000},{<<"hidden">>,false}]]}}
, {ddView,5074168,undefined,system,<<"egambo.resume(GameId)">>,110758147,{viewstate,[{<<"plane_specs">>,[[{<<"script">>,<<>>}]]},{<<"plane_to_show">>,0}],[]}}
, {ddView,20153176,undefined,system,<<"egambo.start(GameType)">>,68318971,{viewstate,[{<<"width">>,458.813},{<<"height">>,100.375},{<<"y">>,0},{<<"x">>,0},{<<"plane_to_show">>,0},{<<"plane_specs">>,[[{<<"script">>,<<>>}]]},{<<"start_btn">>,<<">">>}],[[{<<"name">>,<<"GameId_1">>},{<<"width">>,400.813},{<<"hidden">>,false}]]}}
, {ddView,16995688,undefined,system,<<"egambo.start(GameType,OpponentAcc)">>,64425379,{viewstate,[{<<"width">>,458.813},{<<"height">>,100.375},{<<"y">>,0},{<<"x">>,0},{<<"plane_to_show">>,0},{<<"plane_specs">>,[[{<<"script">>,<<>>}]]},{<<"start_btn">>,<<">">>}],[[{<<"name">>,<<"GameId_1">>},{<<"width">>,268.84340000000003},{<<"hidden">>,false}]]}}
, {ddView,80862738,undefined,system,<<"egambo.d3.Board(GameId)">>,30917719,{viewstate,[{<<"width">>,971.8},{<<"height">>,482.4},{<<"y">>,0},{<<"x">>,365},{<<"plane_to_show">>,1},{<<"plane_specs">>,
[[{<<"script">>,
    <<"function init(container, width, height) {\n    \"use strict\";\n    // This code is executed once and it should initialize the graph, the\n    // available parameters are:\n\n\n    // Run the query:\n    // select time, gid, msgtype, to_json(message) as msg from egGameMsg where gid = :integer_GameId\n\n    // virtual coordinates drawing arc radius\n    var size = 1000;\n    // token radius in virtual coordinates\n    var nradius = 50;\n    var animDuration = 500;\n\n    var parseMessage = function(message) {\n        try {\n            return JSON.parse(message);\n        } catch (e) {\n            throw parseError(message);\n        }\n    };\n\n    function extractBoard(rows) {\n        var row = rows.pop();\n        var msg = parseMessage(row.msg_4);\n        var board = [];\n        for(var i = 0; i < msg.height; ++i){\n            for(var j = 0; j < msg.width; ++j) {\n                var id = i*msg.width+j;\n                var token = msg.board[id];\n                board.push({\n                    id: id,\n                    gid: row.gid_2,\n                    token: msg.board[id],\n                    status: msg.status,\n                    x: j*(nradius*2),\n                    y: i*(nradius*2)\n                });\n            }\n        }\n        return board;\n    }\n\n\n    var vBox = {\n        x: -nradius * 2,\n        y: -nradius * 2,\n        w: size + nradius * 2,\n        h: size + nradius * 2\n    };\n\n    var margin = { top: 10, right: 10, bottom: 10, left: 10 }; // physical margins in px\n\n    var svg = container\n        .append('svg')\n        .attr('viewBox', vBox.x + ' ' + vBox.y + ' ' + vBox.w + ' ' + vBox.h)\n        .attr('preserveAspectRatio', 'xMidYMax meet')\n        .style('margin-top', margin.top + 'px')\n        .style('margin-right', margin.right + 'px')\n        .style('margin-bottom', margin.bottom + 'px')\n        .style('margin-left', margin.left + 'px');\n\n    function resize(w, h) {\n        var cheight = h - (margin.top + margin.bottom);\n        var cwidth = w - (margin.left + margin.right);\n        svg.attr('width', cwidth)\n            .attr('height', cheight);\n    }\n\n    resize(width, height);\n\n    var tokenColor = {\n        X: \"#DB210E\",\n        O: \"#FFFF21\",\n        \" \": \"white\"\n    }\n    \n    function move(d) {\n        if(d.token === \" \" && d.status === \"playing\") {\n            helper.browse('egambo.play(GameId,CellInt)', {\n                ':integer_GameId': {typ: \"integer\", val: d.gid},\n                ':integer_CellId': {typ: \"integer\", val: d.id.toString()}\n            });\n        }\n    }\n\n    var firstData = true;\n    return {\n        on_data: function(data) {\n            if(data.length === 0) {\n                return;\n            }\n\n            // Process data and add draw here.\n            console.log(\"the new data arrived\", data);\n            var board = extractBoard(data);\n            svg.selectAll('circle')\n                .data(board, function(d) { return d.id; })\n                .enter()\n                .append('circle')\n                .attr('r', nradius)\n                .attr('cx', function(d) { return d.x; })\n                .attr('cy', function(d) { return d.y; })\n                .style('stroke', 'black')\n                .style('stroke-width', 3)\n                .on('click', move);\n            \n            // Update new moves\n            svg.selectAll('circle')\n                .style('fill', function(d) {\n                    return tokenColor[d.token];\n                })\n        },\n        on_resize: resize,\n        on_reset: function() {\n            // Called when the button clear the graph is clicked.\n            svg.selectAll('svg > *').remove();\n        },\n        on_close: function() {\n            // This should cleanup event listeners and element added\n            // outside the container, the container itself will be removed\n            // after this function call.\n        }\n    };\n}\n">>}]]},
{<<"start_btn">>,<<"pt">>}],
[[{<<"name">>,<<"time_1">>},{<<"width">>,218.4},{<<"hidden">>,false}],[{<<"name">>,<<"gid_2">>},{<<"width">>,151.2},{<<"hidden">>,false}],[{<<"name">>,<<"msgtype_3">>},{<<"width">>,109.2},{<<"hidden">>,false}],[{<<"name">>,<<"msg_4">>},{<<"width">>,1000},{<<"hidden">>,false}]]}}
, {ddView,131480912,undefined,system,<<"egambo.d3.MyGames">>,91728054,{viewstate,[{<<"width">>,646},{<<"height">>,257},{<<"y">>,381},{<<"x">>,367},{<<"plane_to_show">>,1},{<<"plane_specs">>,
[[{<<"script">>,
    <<"function init(container, width, height) {\n    \"use strict\";\n    // This code is executed once and it should initialize the graph, the\n    // available parameters are:\n\n    // Run the query:\n    // select gid, tid, cid, players, bots, ctime, ialiases, imovers, space, stime, etime, status, nmovers, naliases, nscores from egGame where hd(players) = user or safe_integer(hd(tl(players))) = user\n\n    // Enabled bots, update as new ones are added.\n    function initGraph() {\n        return {};\n    }\n\n    function extractGameList(rows, list) {\n        rows.forEach(function(row) {\n            list[row.gid_1] = {\n                name: row.tid_2,\n                movers: row.nmovers_13,\n                alias: row.naliases_14,\n                status: row.status_12\n            };\n        });\n        return list;\n    }\n\n    // Background and viewport settings\n    var viewport = container\n        .append('div')\n        .style('font-size', '16px')\n        .style('font-famlily', 'Impact')\n        .style('background-color', '#1C848B')\n        .style('color', 'black');\n    \n    // Header of the game list table\n    var header = viewport\n        .append('div')\n        .style('width', '100%')\n        .style('background-color', '#B7B7B7')\n        .style('height', '25px')\n        .style('border-bottom', '1px black dashed');\n    \n    // Header content\n    header\n        .append('span')\n        .style('display', 'inline-block')\n        .style('width', '30%')\n        .style('text-align', 'center')\n        .text('Game Id');\n    \n    header\n        .append('span')\n        .style('display', 'inline-block')\n        .style('width', '20%')\n        .style('text-align', 'center')\n        .style('border-left', '1px solid')\n        .text('Name');\n    \n    header\n        .append('span')\n        .style('display', 'inline-block')\n        .style('width', '15%')\n        .style('text-align', 'center')\n        .style('border-left', '1px solid')\n        .text('Movers');\n    \n    header\n        .append('span')\n        .style('display', 'inline-block')\n        .style('width', '15%')\n        .style('text-align', 'center')\n        .style('border-left', '1px solid')\n        .text('Alias');\n    \n    header\n        .append('span')\n        .style('display', 'inline-block')\n        .style('width', '19%')\n        .style('text-align', 'center')\n        .style('border-left', '1px solid')\n        .text('Status');\n\n    // Section for the content of the game list\n    var content = viewport\n        .append('div')\n        .style('height', 'calc(100% - 25px)')\n        .style('overflow-y', 'auto');\n\n    function openGame(d) {\n        helper.browse('egambo.d3.Board(GameId)', {\n            ':integer_GameId': {typ: 'integer', val: d.id}\n        });\n    }\n\n    function resize(w, h) {\n        viewport\n            .style('width', w + \"px\")\n            .style('height', h + \"px\");\n    }\n\n    function entriesByStatus(obj) {\n        var res = {\n            playing: [],\n            forming: [],\n            finished: []\n        };\n        for(var k in obj) {\n            obj[k].id = k;\n            var status = obj[k].status;\n            if(status === 'playing' || status === 'forming' || status === 'finished') {\n                res[status].push(obj[k]);\n            }\n        }\n        return res;\n    }\n\n    resize(width, height);\n    \n    var gameList = initGraph();\n    return {\n        on_data: function(rows) {\n            if(rows.length === 0) {\n                return;\n            }\n\n            // Process rows and add draw here.\n            console.log(\"the new rows arrived\", rows);\n            gameList = extractGameList(rows, gameList);\n\n            var listByStatus = entriesByStatus(gameList);\n\n            // Add other status entries too...\n            var games = content.selectAll('div')\n                .data(listByStatus.playing, function(d) { return d.id; });\n            \n            games.exit().remove();\n\n            var newGames = games.enter()\n                .append('div')\n                .style('width', '100%')\n                .style('background-color', 'white');\n\n            newGames\n                .append('span')\n                .style('display', 'inline-block')\n                .style('width', '30%')\n                .style('text-align', 'center')\n                .style('border-right', '1px solid')\n                .style('-webkit-user-select', 'text')  /* Chrome 49+ */\n                .style('-moz-user-select', 'text')     /* Firefox 43+ */\n                .style('-ms-user-select', 'text')      /* No support yet */\n                .style('user-select', 'text')\n                .attr('class', 's-playing-id');\n            \n            newGames\n                .append('span')\n                .style('display', 'inline-block')\n                .style('width', '20%')\n                .style('text-align', 'center')\n                .style('border-right', '1px solid')\n                .attr('class', 's-playing-name');\n            \n            newGames\n                .append('span')\n                .style('display', 'inline-block')\n                .style('width', '15%')\n                .style('text-align', 'center')\n                .style('border-right', '1px solid')\n                .attr('class', 's-playing-movers');\n            \n            newGames\n                .append('span')\n                .style('display', 'inline-block')\n                .style('width', '15%')\n                .style('text-align', 'center')\n                .style('border-right', '1px solid')\n                .attr('class', 's-playing-alias');\n            \n            newGames\n                .append('span')\n                .style('display', 'inline-block')\n                .style('width', '19%')\n                .style('text-align', 'center')\n                .style('cursor', 'pointer')\n                .on('click', openGame)\n                .attr('class', 's-playing-status');\n\n            // Upate information for existing games\n            var allGames = content.selectAll('div');\n\n            allGames.select('.s-playing-id')\n                .text(function(d) {\n                    return d.id;\n                });\n\n            allGames.select('.s-playing-name')\n                .text(function(d) {\n                    return d.name;\n                });\n\n            allGames.select('.s-playing-movers')\n                .text(function(d) {\n                    return d.movers;\n                });\n\n            allGames.select('.s-playing-alias')\n                .text(function(d) {\n                    return d.alias;\n                });\n\n            allGames.select('.s-playing-status')\n                .style('background-color', '#2DD962')\n                .style('border-radius', '50%')\n                .text(function(d) {\n                    return d.status;\n                });\n\n        },\n        on_resize: resize,\n        on_reset: function() {\n            // Called when the button clear the graph is clicked.\n            viewport.selectAll('div > *').remove();\n            gameList = initGraph();\n        },\n        on_close: function() {\n            // This should cleanup event listeners and element added\n            // outside the container, the container itself will be removed\n            // after this function call.\n        }\n    };\n}\n">>}]]},
{<<"start_btn">>,<<"pt">>}],
[[{<<"name">>,<<"gid_1">>},{<<"width">>,159.6},{<<"hidden">>,false}],[{<<"name">>,<<"tid_2">>},{<<"width">>,100.8},{<<"hidden">>,false}],[{<<"name">>,<<"cid_3">>},{<<"width">>,134.4},{<<"hidden">>,false}],[{<<"name">>,<<"players_4">>},{<<"width">>,67},{<<"hidden">>,false}],[{<<"name">>,<<"bots_5">>},{<<"width">>,243.60000000000002},{<<"hidden">>,false}],[{<<"name">>,<<"ctime_6">>},{<<"width">>,218.4},{<<"hidden">>,false}],[{<<"name">>,<<"ialiases_7">>},{<<"width">>,73},{<<"hidden">>,false}],[{<<"name">>,<<"imovers_8">>},{<<"width">>,67},{<<"hidden">>,false}],[{<<"name">>,<<"space_9">>},{<<"width">>,55},{<<"hidden">>,false}],[{<<"name">>,<<"stime_10">>},{<<"width">>,218.4},{<<"hidden">>,false}],[{<<"name">>,<<"etime_11">>},{<<"width">>,218.4},{<<"hidden">>,false}],[{<<"name">>,<<"status_12">>},{<<"width">>,67.2},{<<"hidden">>,false}],[{<<"name">>,<<"nmovers_13">>},{<<"width">>,67},{<<"hidden">>,false}],[{<<"name">>,<<"naliases_14">>},{<<"width">>,73},{<<"hidden">>,false}],[{<<"name">>,<<"nscores_15">>},{<<"width">>,67},{<<"hidden">>,false}]]}}
, {ddView,64302096,undefined,system,<<"egambo.d3.Start">>,74098184,{viewstate,[{<<"width">>,368},{<<"height">>,427.4},{<<"y">>,85.60000610351563},{<<"x">>,0},{<<"plane_to_show">>,1},{<<"plane_specs">>,
[[{<<"script">>,
    <<"function init(container, width, height) {\n    \"use strict\";\n    // This code is executed once and it should initialize the graph, the\n    // available parameters are:\n\n\n    // Run the query:\n    // select tid, tname from egambo.egGameType\n\n    // Enabled bots, update as new ones are added.\n    var opponents = [\n        {id: 1, name: \"Bot 1\"},\n        {id: 2, name: \"Bot 2\"},\n        {id: -1, name: 'None'},\n        {id: 5, name: \"Connect 4 Bot\"}\n    ];\n\n    var opponent = 1;\n\n    function initGraph() {\n        return {};\n    }\n\n    function extractGameTypes(rows, types) {\n        rows.forEach(function(row) {\n            types[row.tid_1] = {\n                name: row.tname_2\n            };\n        });\n        return types;\n    }\n\n    // Background and viewport settings\n    var viewport = container\n        .append('div')\n        .style('padding', '25px 0px 0px 0px')\n        .style('font-size', '20px')\n        .style('font-famlily', 'Impact')\n        .style('background-color', '#1C848B')\n        .style('color', 'black');\n\n    // Opponent title\n    viewport\n        .append('div')\n        .style('background-color', '#B7B7B7')\n        .style('width', '60%')\n        .style('height', '32px')\n        .style('border-radius', '50px')\n        .style('border', '2px dashed black')\n        .style('text-align', 'center')\n        .style('margin-left', '20%')\n        .style('margin-bottom', '30px')\n        .text('Opponent');\n    \n    // Opponent section\n    var opponentDiv = viewport\n        .append('div')\n        .style('margin-bottom', '30px')\n        .style('margin-left', '20%')\n        .style('width', '60%')\n        .style('display', 'inline-block');\n    \n    /*var opponentSelect = viewport\n        .append('select')\n        .style('width', '60%')\n        .style('margin-left', '20%')\n        .style('margin-bottom', '50px')\n        .style('font-size', '14px');\n    */\n\n    // Game type title\n    viewport\n        .append('div')\n        .style('background-color', '#B7B7B7')\n        .style('width', '60%')\n        .style('height', '32px')\n        .style('border-radius', '50px')\n        .style('border', '2px dashed black')\n        .style('text-align', 'center')\n        .style('margin-left', '20%')\n        .style('margin-bottom', '30px')\n        .text('Game Type');\n    \n    // Game type select\n    var typesSelect = viewport\n        .append('select')\n        .style('width', '60%')\n        .style('margin-left', '20%')\n        .style('margin-bottom', '30px')\n        .style('font-size', '14px');\n    \n    // Start button\n    viewport.append('div')\n        .style('background-color', '#2DD962')\n        .style('width', '40%')\n        .style('border-radius', '50%')\n        .style('text-align', 'center')\n        .style('margin-left', '30%')\n        .style('cursor', 'pointer')\n        .on('click', startGame)\n        .text('Start');\n\n    function startGame() {\n        var gameType = typesSelect.property('value');\n        var parameters = {':binstr_GameType': {typ: 'binstr', val: gameType}};\n        var view = 'egambo.start(GameType)';\n        if(opponent !== -1) {\n            parameters[':integer_OpponentAccountId'] = {typ: 'integer', val: opponent.toString()};\n            view = 'egambo.start(GameType,OpponentAcc)';\n        }\n        helper.browse(view, parameters);\n    }\n\n    function resize(w, h) {\n        viewport\n            .style('width', w + \"px\")\n            .style('height', h + \"px\");\n    }\n\n    function entries(obj) {\n        var res = [];\n        for(var k in obj) {\n            obj[k].id = k;\n            res.push(obj[k]);\n        }\n        return res;\n    }\n\n    function applyOpponentStyle() {\n        opponentDiv.selectAll('span')\n            .style('background-color', function(d) {\n                if(d.id === opponent) {\n                    return 'orange';\n                }\n                return 'grey';\n            })\n            .style('color', function(d) {\n                if(d.id === opponent) {\n                    return 'white';\n                }\n                return 'black';\n            });\n    }\n\n    function selectOpponent(d) {\n        opponent = d.id;\n        applyOpponentStyle();\n    }\n\n    resize(width, height);\n    \n    var gameTypes = initGraph();\n    return {\n        on_data: function(rows) {\n            if(rows.length === 0) {\n                return;\n            }\n\n            // Set opponent static data for now...\n            /*opponentSelect.selectAll('option')\n                .data(opponents, function(d) { return d.id; })\n                .enter()\n                .append('option')\n                .text(function(d) { return d.name; });\n            */\n\n            // Add new opponents\n            opponentDiv.selectAll('span')\n                .data(opponents, function(d) { return d.id; })\n                .enter()\n                .append('span')\n                .style('cursor', 'pointer')\n                .style('width', function(d) {\n                    if(d.id === 5) {\n                        return '90%';\n                    }\n                    return '30%';\n                })\n                .style('text-align', 'center')\n                .style('font-size', '18px')\n                .style('height', '28px')\n                .style('padding-top', '3px')\n                .style('display', 'inline-block')\n                .style('border-radius', '20px 0px')\n                .style('margin-left', function(d) {\n                    if(d.id === 1) {\n                        return '0px';\n                    }\n                    return '5%';\n                })\n                .style('overflow', 'hidden')\n                .style('margin-bottom', '5px')\n                .style('float', 'left')\n                .on('click', selectOpponent)\n                .text(function(d) { return d.name; });\n            \n            // Apply selected opponent style to selected button\n            applyOpponentStyle();\n\n            // Process rows and add draw here.\n            console.log(\"the new rows arrived\", rows);\n            gameTypes = extractGameTypes(rows, gameTypes);\n\n            typesSelect.selectAll('option')\n                .data(entries(gameTypes), function(d) { return d.id; })\n                .enter()\n                .append('option')\n                .attr('value', function(d) { return d.id; })\n                .text(function(d) { return d.name; });\n        },\n        on_resize: resize,\n        on_reset: function() {\n            // Called when the button clear the graph is clicked.\n            viewport.selectAll('div > *').remove();\n            gameTypeList = initGraph();\n        },\n        on_close: function() {\n            // This should cleanup event listeners and element added\n            // outside the container, the container itself will be removed\n            // after this function call.\n        }\n    };\n}\n">>
    }]]},
{<<"start_btn">>,<<">">>}],
[[{<<"name">>,<<"tid_1">>},{<<"width">>,151.2},{<<"hidden">>,false}],[{<<"name">>,<<"tname_2">>},{<<"width">>,352.8},{<<"hidden">>,false}]]}}
, {ddView,97251192,undefined,system,<<"egambo.d3.Board(Pattern)">>,123728987,{viewstate,[{<<"width">>,1434.6},{<<"height">>,473.4},{<<"y">>,304},{<<"x">>,430},{<<"plane_to_show">>,1},{<<"plane_specs">>,
[[{<<"script">>,
    <<"function init(container, width, height) {\n    \"use strict\";\n    // This code is executed once and it should initialize the graph, the\n    // available parameters are:\n\n    // Expected data w, h, data\n\n    // query:\n    // select 9 as w, 1 as h, :binstr_pattern as data from dual\n\n    // virtual coordinates drawing arc radius\n    var size = 1000;\n    // token radius in virtual coordinates\n    var nradius = 50;\n    var animDuration = 500;\n\n    var vBox = {\n        x: -nradius * 2,\n        y: -nradius * 2,\n        w: size + nradius * 2,\n        h: size + nradius * 2\n    };\n\n    var margin = { top: 10, right: 10, bottom: 10, left: 10 }; // physical margins in px\n\n    var svg = container\n        .append('svg')\n        .attr('viewBox', vBox.x + ' ' + vBox.y + ' ' + vBox.w + ' ' + vBox.h)\n        .attr('preserveAspectRatio', 'xMidYMax meet')\n        .style('margin-top', margin.top + 'px')\n        .style('margin-right', margin.right + 'px')\n        .style('margin-bottom', margin.bottom + 'px')\n        .style('margin-left', margin.left + 'px');\n\n    var parseMessage = function(message) {\n        // Remove erlang binary delimitators\n        message = message.replace(\"<<\", \"\").replace(\">>\", \"\");\n        message = message.split(\"{\").join(\"[\").split(\"}\").join(\"]\");\n        try {\n            return JSON.parse(message);\n        } catch (e) {\n            return message;\n        }\n    };\n\n    var max;\n    function extractBoard(rows) {\n        max = 0;\n        var row = rows.pop();\n        var data = parseMessage(row.data_3);\n        var height = row.h_2;\n        var width = row.w_1;\n        var board = [];\n        // Fit the data to the screen\n        var sizeW = width * nradius * 2;\n        var sizeH = height * nradius * 2;\n        var vBox = {\n            x: -nradius * 2,\n            y: -nradius * 2,\n            w: sizeW + nradius * 2,\n            h: sizeH + nradius * 2\n        };\n        svg.attr('viewBox', vBox.x + ' ' + vBox.y + ' ' + vBox.w + ' ' + vBox.h);\n\n\n        if(Array.isArray(data[0])) {\n            data.reverse();\n            // Initialize an empty board before adding the moves...\n            for(var i = 0; i < height*width; ++i) {\n                board.push({\n                    id: i,\n                    token: \" \",\n                    label: \" \",\n                    x: (i % width) * (nradius * 2),\n                    y: Math.floor(i/width) * (nradius * 2)\n                })\n            }\n            for(var i = 0; i < data.length; ++i) {\n                var token = String.fromCharCode(data[i][0]);\n                var id = data[i][1];\n                var x = (id % width) * (nradius * 2);\n                var y = Math.floor(id/width) * (nradius * 2);\n                board[id] = {\n                    id: id,\n                    token: token,\n                    label: token + \" \" + (i+1),\n                    x: x,\n                    y: y\n                };\n            }\n        } else {\n            for(var i = 0; i < height; ++i) {\n                for(var j = 0; j < width; ++j) {\n                    var id = i * width + j;\n                    var token = data[id];\n                    if (Math.abs(token) > max) {\n                        max = Math.abs(token);\n                    }\n                    board.push({\n                        id: id,\n                        token: token,\n                        label: token,\n                        x: j * (nradius * 2),\n                        y: i * (nradius * 2)\n                    });\n                }\n            }\n        }\n        return board;\n    }\n\n    function resize(w, h) {\n        var cheight = h - (margin.top + margin.bottom);\n        var cwidth = w - (margin.left + margin.right);\n        svg.attr('width', cwidth)\n            .attr('height', cheight);\n    }\n\n    resize(width, height);\n\n    var tokenColor = {\n        X: \"#DB210E\",\n        O: \"#3352FF\",\n        $: \"black\",\n        \"*\": \"#2BE9A7\",\n        \" \": \"white\"\n    }\n\n    function getColor(token) {\n        if(tokenColor[token]) {\n            return tokenColor[token];\n        } else if(token < 0) {\n            return tokenColor.X;\n        } else if(token > 0) {\n            return tokenColor.O;\n        } else {\n            return tokenColor[\" \"];\n        }\n    }\n\n    function getSize(token) {\n        if(tokenColor[token]) {\n            return nradius;\n        } else {\n            var scaled = Math.max(Math.abs(token) * nradius / max, 1);\n            if (Number.isNaN(scaled)) {\n                return nradius;\n            } else {\n                return scaled;\n            }\n        }\n    }\n\n    function getLabel(d) {\n        var f = +d.label;\n        if(Number.isNaN(f) || d.label === \" \") {\n            return d.label;\n        }\n        return f.toFixed(1);\n    }\n\n    var firstData = true;\n    return {\n        on_data: function(data) {\n            if(data.length === 0) {\n                return;\n            }\n\n            // Process data and add draw here.\n            console.log(\"the new data arrived\", data);\n            var board = extractBoard(data);\n            svg.selectAll('circle')\n                .data(board, function(d) { return d.id; })\n                .enter()\n                .append('circle')\n                .attr('r', function(d) {\n                    return getSize(d.token);\n                })\n                .attr('cx', function(d) { return d.x; })\n                .attr('cy', function(d) { return d.y; })\n                .style('stroke', 'black')\n                .style('stroke-width', 3)\n                .style('fill', function(d) {\n                    return getColor(d.token);\n                });\n\n            // Add label\n            svg.selectAll('text')\n                .data(board, function(d) { return d.id; })\n                .enter()\n                .append('text')\n                .style('font-size', '24px')\n                .attr('text-anchor', 'middle')\n                .text(getLabel)\n                .attr('x', function(d) { return d.x; })\n                .attr('y', function(d) { return d.y + 8; });\n        },\n        on_resize: resize,\n        on_reset: function() {\n            // Called when the button clear the graph is clicked.\n            svg.selectAll('svg > *').remove();\n        },\n        on_close: function() {\n            // This should cleanup event listeners and element added\n            // outside the container, the container itself will be removed\n            // after this function call.\n        }\n    };\n}\n">>
    }]]},
{<<"start_btn">>,<<">">>}],
[[{<<"name">>,<<"w_1">>},{<<"width">>,31},{<<"hidden">>,false}],[{<<"name">>,<<"h_2">>},{<<"width">>,31},{<<"hidden">>,false}],[{<<"name">>,<<"data_3">>},{<<"width">>,1312.6},{<<"hidden">>,false}]]}}
]).

-define(DEFAULT_CMDS, [
  {ddCmd,112552870,<<"All ddViews">>,system,[oci,imem],local,<<"select c.owner, v.name from ddView v, ddCmd c where c.id = v.cmd and (c.conns = to_list('[]') or is_member(:ddConn.id, c.conns)) and (c.owner = user or c.owner = to_atom('system')) and is_member(:ddAdapter.id, c.adapters) order by 2 asc, 1 asc">>,[]}
, {ddCmd,94732307,<<"Remote Tables">>,system,[oci],[],<<"select concat(OWNER,concat('.', TABLE_NAME)) as QUALIFIED_TABLE_NAME from ALL_TABLES where OWNER=user order by TABLE_NAME">>,[]}
, {ddCmd,49488724,<<"Remote Tables">>,system,[imem],[],<<"select\n                to_name(qname),\n                size as rows,\n                memory,\n                nodef(expiry) as expires,\n                nodef(tte) as expires_after\n            from\n                all_tables,\n                ddSize\n            where\n                name = element(2, qname) and size <> to_atom('undefined')\n            order by 1 asc">>,[]}
, {ddCmd,123610590,<<"Remote Users">>,system,[oci],[],<<"select USERNAME from ALL_USERS">>,[]}
, {ddCmd,111381517,<<"Remote Views">>,system,[oci],[],<<"select concat(OWNER,concat('.', VIEW_NAME)) as QUALIFIED_TABLE_NAME from ALL_VIEWS where OWNER=user order by VIEW_NAME">>,[]}
, {ddCmd,98509861,<<"ddAccount">>,system,[imem],[],<<"select id, name, type, credentials, fullName, lastLoginTime, lastFailureTime, lastPasswordChangeTime, locked from egambo.ddAccount">>,[]}
, {ddCmd,90158797,<<"ddConfig">>,system,[imem],[],<<"select hkl, val, owner, remark from ddConfig">>,[]}
, {ddCmd,98106028,<<"ddRole">>,system,[imem],[],<<"select id, \"roles\", permissions, quotas from ddRole">>,[]}
, {ddCmd,27433988,<<"egambo.create(GameType,Cnt,OpponentAcc)">>,system,[imem],[],<<"select mfa('egambo_game', 'create', list(t.item, cnt.item, ya.item, user)) as GameIDs from term t, integer cnt, integer ya where t.item = :binstr_GameType and cnt.item = :integer_GameCount and ya.item = :integer_OpponentAccountId">>,[]}
, {ddCmd,27253828,<<"egambo.create(GameType,Cnt,OpponentAcc,OwnerAcc)">>,system,[imem],[],<<"select mfa('egambo_game', 'create', list(t.item, cnt.item, opponent.item, owner.item)) as GameIDs from term t, integer cnt, integer opponent, integer owner where t.item = :binstr_GameType and cnt.item = :integer_GameCount and opponent.item = :integer_OpponentAccountId and owner.item = :integer_OwnerAccountId">>,[]}
, {ddCmd,72273526,<<"egambo.egGame">>,system,[imem],[],<<"select gid, tid, players, bots, ctime, ialiases, imovers, space, stime, etime, status, board, nmovers, naliases, nscores, moves from egambo.egGame">>,[]}
, {ddCmd,91864289,<<"egambo.egGame.Playing">>,system,[imem],[],<<"select gid, tid, players, bots, ctime, ialiases, imovers, space, stime, etime, status, board, nmovers, naliases, nscores, moves from egambo.egGame where egambo.egGame.status = to_atom('playing')">>,[]}
, {ddCmd,75649981,<<"egambo.egGame.Recent">>,system,[imem],[],<<"select gid, tid, imovers, status, board, nmovers, naliases, nscores, moves from egambo.egGame where ctime > systimestamp - 1 / 24">>,[]}
, {ddCmd,8606876,<<"egambo.egGame.Sampler(GameType)">>,system,[imem],[],<<"select gid, tid, imovers, status, board, nmovers, naliases, nscores, mfa('egambo_tictac', 'sample', list(space, ialiases, moves, naliases, nscores)) as sample, moves from egambo.egGame where egambo.egGame.tid = :binstr_GameType and egambo.egGame.nscores <> to_list('[0,0]') and length(moves) <= 9 and status = to_atom('finished')">>,[]}
, {ddCmd,12106255,<<"egambo.egGame.Unfinished">>,system,[imem],[],<<"select gid, tid, bots, status, board, nmovers, naliases, moves from egambo.egGame where egambo.egGame.status <> to_atom('finished')">>,[]}
, {ddCmd,25460815,<<"egambo.egGame.Width13">>,system,[imem],[],<<"select gid, tid, status, nmovers, naliases, list_to_binstr(list(65 + item)) as row, slice(board, 13 * item + 1, 1) as a, slice(board, 13 * item + 2, 1) as b, slice(board, 13 * item + 3, 1) as c, slice(board, 13 * item + 4, 1) as d, slice(board, 13 * item + 5, 1) as e, slice(board, 13 * item + 6, 1) as f, slice(board, 13 * item + 7, 1) as g, slice(board, 13 * item + 8, 1) as h, slice(board, 13 * item + 9, 1) as i, slice(board, 13 * item + 10, 1) as j, slice(board, 13 * item + 11, 1) as k, slice(board, 13 * item + 12, 1) as l, slice(board, 13 * item + 13, 1) as m from egambo.egGame, integer where item >= 0 and item < 13 and egambo.egGame.gid = :integer_GameId">>,[]}
, {ddCmd,64086186,<<"egambo.egGame.Width15">>,system,[imem],[],<<"select gid, tid, status, nmovers, naliases, list_to_binstr(list(65 + item)) as row, slice(board, 15 * item + 1, 1) as a, slice(board, 15 * item + 2, 1) as b, slice(board, 15 * item + 3, 1) as c, slice(board, 15 * item + 4, 1) as d, slice(board, 15 * item + 5, 1) as e, slice(board, 15 * item + 6, 1) as f, slice(board, 15 * item + 7, 1) as g, slice(board, 15 * item + 8, 1) as h, slice(board, 15 * item + 9, 1) as i, slice(board, 15 * item + 10, 1) as j, slice(board, 15 * item + 11, 1) as k, slice(board, 15 * item + 12, 1) as l, slice(board, 15 * item + 13, 1) as m, slice(board, 15 * item + 14, 1) as n, slice(board, 15 * item + 15, 1) as o from egambo.egGame, integer where item >= 0 and item < 15 and egambo.egGame.gid = :integer_GameId">>,[]}
, {ddCmd,66113065,<<"egambo.egGame.Width19">>,system,[imem],[],<<"select gid, tid, status, nmovers, naliases, list_to_binstr(list(65 + item)) as row, slice(board, 19 * item + 1, 1) as a, slice(board, 19 * item + 2, 1) as b, slice(board, 19 * item + 3, 1) as c, slice(board, 19 * item + 4, 1) as d, slice(board, 19 * item + 5, 1) as e, slice(board, 19 * item + 6, 1) as f, slice(board, 19 * item + 7, 1) as g, slice(board, 19 * item + 8, 1) as h, slice(board, 19 * item + 9, 1) as i, slice(board, 19 * item + 10, 1) as j, slice(board, 19 * item + 11, 1) as k, slice(board, 19 * item + 12, 1) as l, slice(board, 19 * item + 13, 1) as m, slice(board, 19 * item + 14, 1) as n, slice(board, 19 * item + 15, 1) as o, slice(board, 19 * item + 16, 1) as p, slice(board, 19 * item + 16, 1) as p, slice(board, 19 * item + 17, 1) as q, slice(board, 19 * item + 18, 1) as r, slice(board, 19 * item + 19, 1) as s from egambo.egGame, integer where item >= 0 and item < 19 and egambo.egGame.gid = :integer_GameId">>,[]}
, {ddCmd,91783774,<<"egambo.egGame.Width3">>,system,[imem],[],<<"select gid, tid, status, nmovers, naliases, list_to_binstr(list(64 + item)) as row, slice(board, 3 * item - 2, 1) as a, slice(board, 3 * item - 1, 1) as b, slice(board, 3 * item, 1) as c from egambo.egGame, integer where item >= 1 and item <= 3 and egambo.egGame.gid = :integer_GameId">>,[]}
, {ddCmd,65884760,<<"egambo.egGame.Width4">>,system,[imem],[],<<"select gid, tid, status, nmovers, naliases, list_to_binstr(list(64 + item)) as row, slice(board, 4 * item - 3, 1) as a, slice(board, 4 * item - 2, 1) as b, slice(board, 4 * item - 1, 1) as c, slice(board, 4 * item, 1) as d from egambo.egGame, integer where item >= 1 and item <= 4 and egambo.egGame.gid = :integer_GameId">>,[]}
, {ddCmd,79512648,<<"egambo.egGame.Width5">>,system,[imem],[],<<"select gid, tid, status, nmovers, naliases, list_to_binstr(list(65 + item)) as row, slice(board, 5 * item + 1, 1) as a, slice(board, 5 * item + 2, 1) as b, slice(board, 5 * item + 3, 1) as c, slice(board, 5 * item + 4, 1) as d, slice(board, 5 * item + 5, 1) as e from egambo.egGame, integer where item >= 0 and item < 5 and egambo.egGame.gid = :integer_GameId">>,[]}
, {ddCmd,93207148,<<"egambo.egGame.Width6">>,system,[imem],[],<<"select gid, tid, status, nmovers, naliases, list_to_binstr(list(65 + item)) as row, slice(board, 6 * item + 1, 1) as a, slice(board, 6 * item + 2, 1) as b, slice(board, 6 * item + 3, 1) as c, slice(board, 6 * item + 4, 1) as d, slice(board, 6 * item + 5, 1) as e, slice(board, 6 * item + 6, 1) as f from egambo.egGame, integer where item >= 0 and item < 6 and egambo.egGame.gid = :integer_GameId">>,[]}
, {ddCmd,23833684,<<"egambo.egGame.Width7">>,system,[imem],[],<<"select gid, tid, status, nmovers, naliases, list_to_binstr(list(65 + item)) as row, slice(board, 7 * item + 1, 1) as a, slice(board, 7 * item + 2, 1) as b, slice(board, 7 * item + 3, 1) as c, slice(board, 7 * item + 4, 1) as d, slice(board, 7 * item + 5, 1) as e, slice(board, 7 * item + 6, 1) as f, slice(board, 7 * item + 7, 1) as g from egambo.egGame, integer where item >= 0 and item < 7 and egambo.egGame.gid = :integer_GameId">>,[]}
, {ddCmd,85560651,<<"egambo.egGame.Width8">>,system,[imem],[],<<"select gid, tid, status, nmovers, naliases, list_to_binstr(list(65 + item)) as row, slice(board, 8 * item + 1, 1) as a, slice(board, 8 * item + 2, 1) as b, slice(board, 8 * item + 3, 1) as c, slice(board, 8 * item + 4, 1) as d, slice(board, 8 * item + 5, 1) as e, slice(board, 8 * item + 6, 1) as f, slice(board, 8 * item + 7, 1) as g, slice(board, 8 * item + 8, 1) as h from egambo.egGame, integer where item >= 0 and item < 8 and egambo.egGame.gid = :integer_GameId">>,[]}
, {ddCmd,102692664,<<"egambo.egGame.Width9">>,system,[imem],[],<<"select gid, tid, status, nmovers, naliases, list_to_binstr(list(65 + item)) as row, slice(board, 9 * item + 1, 1) as a, slice(board, 9 * item + 2, 1) as b, slice(board, 9 * item + 3, 1) as c, slice(board, 9 * item + 4, 1) as d, slice(board, 9 * item + 5, 1) as e, slice(board, 9 * item + 6, 1) as f, slice(board, 9 * item + 7, 1) as g, slice(board, 9 * item + 8, 1) as h, slice(board, 9 * item + 9, 1) as i from egambo.egGame, integer where item >= 0 and item < 9 and egambo.egGame.gid = :integer_GameId">>,[]}
, {ddCmd,12450316,<<"egambo.egGameCategory">>,system,[imem],[],<<"select cid, cname, info from egambo.egGameCategory">>,[]}
, {ddCmd,44230220,<<"egambo.egGameMsg">>,system,[imem],[],<<"select time, gid, msgtype, message from egambo.egGameMsg">>,[]}
, {ddCmd,119553388,<<"egambo.egGameMsg.Recent">>,system,[imem],[],<<"select time, gid, msgtype, message from egambo.egGameMsg where time > systimestamp - 1 / 24">>,[]}
, {ddCmd,33484121,<<"egambo.egGameType">>,system,[imem],[],<<"select tid, tname, cid, engine, players, params, setup, level, info from egambo.egGameType">>,[]}
, {ddCmd,11410941,<<"egambo.play(GameId,CellAtom)">>,system,[imem],[],<<"select mfa('egambo_game', 'play', list(game.item, cell.item, user)) as Result from integer game, atom cell where game.item = :integer_GameId and cell.item = :atom_CellId">>,[]}
, {ddCmd,125286160,<<"egambo.play(GameId,CellInt)">>,system,[imem],[],<<"select\n      mfa\n        (\n          'egambo_game'\n          ,\n          'play'\n          ,\n            list(game.item,cell.item, user) \n        ) Result\n  from\n    integer game\n    ,\n    integer cell\n  where\n    game.item = :integer_GameId \n    and\n    cell.item = :integer_CellId \n">>,[]}
, {ddCmd,4333626,<<"egambo.play_all(GameId,CellAtom)">>,system,[imem],[],<<"select mfa('egambo_game', 'play', list(game.item, cell.item)) as Result from integer game, atom cell where game.item = :integer_GameId and cell.item = :atom_CellId">>,[]}
, {ddCmd,23246841,<<"egambo.play_all(GameId,CellInt)">>,system,[imem],[],<<"select mfa('egambo_game', 'play', list(game.item, cell.item)) as Result from integer game, integer cell where game.item = :integer_GameId and cell.item = :integer_CellId">>,[]}
, {ddCmd,70332437,<<"egambo.result(GameId)">>,system,[imem],[],<<"select to_json(mfa('egambo_game', 'result', list(item))) as Result from integer where item = :integer_GameId">>,[]}
, {ddCmd,110758147,<<"egambo.resume(GameId)">>,system,[imem],[],<<"select to_json(mfa('egambo_game', 'resume', list(item))) as Result from integer where item = :integer_GameId">>,[]}
, {ddCmd,68318971,<<"egambo.start(GameType)">>,system,[imem],[],<<"select mfa('egambo_game', 'start', list(t.item, user)) as GameId from term t where t.item = :binstr_GameType">>,[]}
, {ddCmd,64425379,<<"egambo.start(GameType,OpponentAcc)">>,system,[imem],[],<<"select mfa('egambo_game', 'start', list(t.item, ya.item, user)) as GameId from term t, integer ya where t.item = :binstr_GameType and ya.item = :integer_OpponentAccountId">>,[]}
, {ddCmd,30917719,<<"egambo.d3.Board(GameId)">>,system,[imem],[],<<"select time, gid, msgtype, to_json(message) as msg from egGameMsg where gid = :integer_GameId">>,[]}
, {ddCmd,91728054,<<"egambo.d3.MyGames">>,system,[imem],[],<<"select gid, tid, cid, players, bots, ctime, ialiases, imovers, space, stime, etime, status, nmovers, naliases, nscores from egGame where hd(players) = user or safe_integer(hd(tl(players))) = user">>,[]}
, {ddCmd,74098184,<<"egambo.d3.Start">>,system,[imem],[],<<"select tid, tname from egambo.egGameType">>,[]}
, {ddCmd,123728987,<<"egambo.d3.Board(Pattern)">>,system,[imem],[],<<"select 9 as w, 1 as h, :binstr_pattern as data from dual">>,[]}
]).

-record(state, {}).

-export([ start_link/0
        ]).

% gen_server behavior callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        , format_status/2
        ]).

-export([ create/2          % unconditionally create a game against anyone, do not try to match existing offerings
        , create/3          % challenge a specific player (accept a reciprocal challenge/matching game or create a new one)
        , create/4          % create multiple challenges in a batch
        , start/2           % want to play against anyone (accept a forming game or create your own)
        , start/3           % want to challenge a specific player (accept a matching offering or create one)
        , cancel/2          % cancel a game in forming state, only possible before playing really starts
        , accept/2          % accept a specific challenge from someone
        , notify/6          % notify players and watchers about a game state change
        , result/1          % return map with current game result (board status, turn, scores)
        , moves/1           % return map with game history
        , read_game/1       % return game status from egGame table
        , read_type/1       % return game status from egGame table
        , write_game/1      % write a game record to the db
        , resume/1          % resume (restart from saved state) one or several games
        , stop/1            % stop (forward stop stignal to engine) one or more games
        , resume_bots/2     % check if bots are running, resume them if necessary
        , eg_time/0         % return system timestamp in the format required for game tables
        , eg_time_to_sec/1
        , eg_time_to_msec/1
        , eg_time_to_usec/1
        ]).

-export([ play/4            % play one move: GameId, Cell|Command, Alias, AccountId     (not going through egambo_game gen_server if engine is running)
        , play/3            % play one move: GameId, Cell|Command, AccountId            (in the name of the account given)
        , play/2            % play one move: GameId, Cell|Command                       (in the name of the next player)
        , play_bot/5        % trigger a bot to play one move (async)
        ]).

-safe([create, start, cancel, accept, play, result, resume, moves, time]).

%% stateful game management functions (run through gen_server for serializability)

-spec create(egGameTypeId(), egAccountId()) -> egGameId() | egGameError().
create(GameType, MyAcc) when is_integer(MyAcc) ->
    gen_server:call(?MODULE, {create, GameType, MyAcc});
create(_, _) -> ?BAD_ACCOUNT.

-spec create(egGameTypeId(), egAccountId(), egAccountId()) -> egGameId() | egGameError().
create(GameType, YourAcc, MyAcc) when is_integer(MyAcc), is_integer(YourAcc) ->
    gen_server:call(?MODULE, {create, GameType, YourAcc, MyAcc});
create(_, _, _) -> ?BAD_ACCOUNT.

-spec create(egGameTypeId(), integer(), egAccountId(), egAccountId()) -> [egGameId() | egGameError()] | egGameError().
create(GameType, Cnt, YourAcc, MyAcc) when is_integer(MyAcc), is_integer(YourAcc), is_integer(Cnt), Cnt > 0, Cnt =< ?MAX_BATCH_COUNT ->
    [gen_server:call(?MODULE, {create, GameType, YourAcc, MyAcc}) || _ <- lists:seq(1, Cnt)];
create(_, _, YourAcc, MyAcc) when is_integer(MyAcc), is_integer(YourAcc) ->  ?BAD_BATCH_START_REQUEST;
create(_, _, _, _) -> ?BAD_ACCOUNT.

-spec start(egGameTypeId(), egAccountId()) -> egGameId() | egGameError().
start(GameType, MyAcc) when is_integer(MyAcc) ->
    gen_server:call(?MODULE, {start, GameType, MyAcc});
start(_, _) -> ?BAD_ACCOUNT.

-spec start(egGameTypeId(), egAccountId(), egAccountId()) -> egGameId() | egGameError().
start(GameType, YourAcc, MyAcc) when is_integer(MyAcc), is_integer(YourAcc) ->
    gen_server:call(?MODULE, {start, GameType, YourAcc, MyAcc});
start(_, _, _) -> ?BAD_ACCOUNT.

-spec cancel(egGameId(), egAccountId()) -> ok | egGameError().
cancel(GameId, MyAcc) when is_integer(MyAcc) ->
    gen_server:call(?MODULE, {cancel, GameId, MyAcc});
cancel(_, _) -> ?BAD_ACCOUNT.

-spec accept(egGameId(), egAccountId()) -> ok | egGameError().
accept(GameId, MyAcc) when is_integer(MyAcc) ->
    gen_server:call(?MODULE, {accept, GameId, MyAcc});
accept(_, _) -> ?BAD_ACCOUNT.

-spec notify(egTime(), egGameId(), egGameMsgType(), egGameMsg(), [egBotId()], [egAccountId()]) -> ok | egGameError().
notify(EventTime, GameId, MessageType, Message, Bots, Players) when is_tuple(EventTime), is_integer(GameId), is_atom(MessageType) ->
    case lists:member(undefined, Bots) of
        true ->
            send_notification(Bots, Players, Message#{type => MessageType}),
            %% TODO: Check real type here as type of time doesn't match,
            %%       egTime == ddTimeUID =/= ddTimestamp
            imem_meta:write(egGameMsg, #egGameMsg{time=EventTime, gid=GameId, msgtype=MessageType, message=Message});
        false ->
            ok  % no notifications/logs needed for games bot against bot
    end.

-spec send_notification([egBotId()], [egAccountId()], map()) -> ok.
send_notification(_, [], _Msg) -> ok;
send_notification([undefined | Bots], [PlayerId | Players], Msg) ->
    egambo_player:notify(PlayerId, Msg),
    send_notification(Bots, Players, Msg);
send_notification([_ | Bots], [_BotId | Players], Msg) ->
    send_notification(Bots, Players, Msg).

-spec resume_bots(egGameTypeId(), [egBotId()]) -> ok | egGameError().
resume_bots(_GameTypeId, []) -> ok;
resume_bots(GameTypeId, [undefined|Bots]) ->
    resume_bots(GameTypeId, Bots);
resume_bots(GameTypeId, [Bot|Bots]) ->
    case global:whereis_name(?BOT_GID(Bot, GameTypeId)) of
        undefined ->
            case Bot:resume(GameTypeId) of
                ok ->       resume_bots(GameTypeId, Bots);
                Error ->    Error
            end;
        Pid when is_pid(Pid) ->
            resume_bots(GameTypeId, Bots)
    end.

-spec play_bot(egBotId(), egGameTypeId(), egGameId(), binary(), [egAlias()]) -> {ok, integer(), binary()} | {error, atom()}.
play_bot(BotId, GameTypeId, GameId, Board, Aliases) ->
        gen_server:cast(?BOT_GID(BotId, GameTypeId), {play_bot_req, GameId, Board, Aliases}).

%% stateless (db direct access) functions

-spec eg_time() -> egTime().
eg_time() -> imem_meta:time_uid().

-spec eg_time_to_sec(egTime()) -> undefined |integer().
eg_time_to_sec(undefined) -> undefined;
eg_time_to_sec({Sec,_,_,_}) -> Sec;
eg_time_to_sec({Sec,_}) -> Sec.

-spec eg_time_to_msec(egTime()) -> undefined |integer().
eg_time_to_msec(undefined) -> undefined;
eg_time_to_msec({Sec,Micro,_,_}) -> 1000*Sec+Micro div 1000;
eg_time_to_msec({Sec,Micro}) -> 1000*Sec+Micro div 1000.

-spec eg_time_to_usec(egTime()) -> undefined |integer().
eg_time_to_usec(undefined) -> undefined;
eg_time_to_usec({Sec,Micro,_,_}) -> 1000000*Sec+Micro;
eg_time_to_usec({Sec,Micro}) -> 1000000*Sec+Micro.

-spec read_game(egGameId()) -> #egGame{} | egGameError().
read_game(GameId) ->
    case catch imem_meta:read(egGame, GameId) of
        [#egGame{} = Game] ->   Game;
        _ ->                    ?NO_SUCH_GAME
    end.

-spec write_game(#egGame{}) -> ok | egGameError().
write_game(Game) ->
    % ToDo: detect a finished game and update global score statsistics
    imem_meta:write(egGame, Game).

-spec read_type(egGameTypeId()) -> #egGameType{} | egGameError().
read_type(GameTypeId) ->
    case catch imem_meta:read(egGameType, GameTypeId) of
        [#egGameType{} = Type] ->   Type;
        _ ->                        ?NO_SUCH_GAME_TYPE
    end.

-spec read_bot(egGameId()) -> #egGame{} | egGameError().
read_bot(AccountId) ->
    case catch imem_meta:read(ddAccount, AccountId) of
        [Account] ->
            case element(4, Account) of
                user ->     undefined;
                deamon ->   undefined;
                Type ->
                    case application:get_key(egambo, modules) of
                        {ok, Modules} ->
                            case lists:member(Type, Modules) of
                                true ->     Type;
                                false ->    undefined
                            end;
                        _ ->
                            undefined
                    end
            end;
        _ ->
            undefined
    end.

-spec start_game(#egGame{}) -> ok | egGameError().
start_game(#egGame{gid=GameId, tid=GameType, bots=Bots, stime=STime, players=Players} = Game) ->
    write_game(Game),
    case read_type(GameType) of
        #egGameType{engine=Engine} ->
          case Engine:resume(GameId) of
              ok ->   notify(STime, GameId, start_success, Engine:result(Game), Bots, Players),
                      ok;
              Err ->  notify(STime, GameId, start_failure, Engine:result(Game), Bots, Players),
                      Err
          end;
        Error ->
          Error
    end.

-spec resume(egGameId() | [egGameId()]) -> ok | egGameError().
resume(GameIds) when is_list(GameIds) ->
    case lists:usort([ resume(ID) || ID <- GameIds]) of
        [ok] -> ok;
        Errlist -> {error, Errlist}
    end;
resume(GameId)  ->
    case read_game(GameId) of
        #egGame{tid=GameType, bots=Bots} ->
            case resume_bots(GameType, Bots) of
                ok ->
                    case read_type(GameType) of
                        #egGameType{engine=Engine} ->   Engine:resume(GameId);
                        Error ->                        Error
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

-spec stop(egGameId() | [egGameId()]) -> ok | egGameError().
stop(GameIds) when is_list(GameIds) ->
    case lists:usort([ stop(ID) || ID <- GameIds]) of
        [ok] -> ok;
        Errlist -> {error, Errlist}
    end;
stop(GameId)  ->
    case read_game(GameId) of
        #egGame{tid=GameType} ->
            case read_type(GameType) of
                #egGameType{engine=Engine} ->   Engine:stop(GameId);
                Error ->                        Error
            end;
        Error ->
            Error
    end.

-spec result(egGameId()) -> egGameResult() | egGameError().
result(GameId) ->
    try
        gen_server:call(?ENGINE_GID(GameId), result)
    catch
        exit:{noproc,_} ->
            case read_game(GameId) of
                #egGame{tid=GameType} = Game ->
                    case read_type(GameType) of
                        #egGameType{engine=Engine} ->   Engine:result(Game);
                        Error ->                        Error
                    end;
                Error ->
                    Error
            end;
        Class:Reason ->
            {error, Class, Reason}
    end.

-spec moves(egGameId()) -> egGameMoves() | egGameError().
moves(GameId) ->
    try
        gen_server:call(?ENGINE_GID(GameId), moves)
    catch
        exit:{noproc,_} ->
            case read_game(GameId) of
                #egGame{tid=GameType} = Game ->
                    case read_type(GameType) of
                        #egGameType{engine=Engine} ->   Engine:moves(Game);
                        Error ->                        Error
                    end;
                Error ->
                    Error
            end
    end.

%% stateless (engine access) functions with fallback to (stateful, serialized) engine creation

-spec play(egGameId(), egGameMove(), egAlias(), egAccountId()) -> ok | game_finished | egGameError().
play(GameId, Move, MyAlias, MyAccountId) ->
    engine_call(GameId, {play, Move, MyAlias, MyAccountId}).

-spec play(egGameId(), egGameMove(), egAccountId()) -> ok | game_finished | egGameError().
play(GameId, Move, MyAccountId) ->
    engine_call(GameId, {play, Move, MyAccountId}).

-spec play(egGameId(), egGameMove()) -> ok | game_finished | egGameError().
play(GameId, Move) ->
    engine_call(GameId, {play, Move}).

engine_call(GameId, Command) ->
    try
        gen_server:call(?ENGINE_GID(GameId), Command)
    catch
        exit:{normal,_} -> game_finished;
        exit:{noproc,_} ->
            case resume(GameId) of
                ok ->
                    try
                        gen_server:call(?ENGINE_GID(GameId), Command)
                    catch
                        _:Err -> Err
                    end;
                {error, {error, Error}} ->
                  {error, Error};
                {error,{{error,Error}, Extra}} ->
                  {error, Error, Extra};
                Error ->
                  Error
            end
    end.

% Internal helper functions

prepare(#egGame{tid=GameType} = Game) ->
    case read_type(GameType) of
        #egGameType{engine=Engine} = Type ->    Engine:prepare(Type, Game);
        _ ->                                    ?NO_SUCH_GAME_TYPE
    end.

insert_default([]) -> ok;
insert_default([Rec|Recs]) ->
    case imem_meta:read(element(1,Rec), element(2,Rec)) of
        [] ->   catch imem_meta:write(element(1,Rec), Rec);
        _ ->    ok
    end,
    insert_default(Recs).

init(_) ->
    Result = try
        imem_meta:init_create_table(egGameCategory, {record_info(fields, egGameCategory), ?egGameCategory, #egGameCategory{}}, [], system),
        imem_meta:init_create_table(egGameType, {record_info(fields, egGameType), ?egGameType, #egGameType{}}, [], system),
        imem_meta:init_create_table(egGame, {record_info(fields, egGame), ?egGame, #egGame{}}, [], system),
        imem_meta:init_create_check_table(egGameMsg, {record_info(fields, egGameMsg), ?egGameMsg, #egGameMsg{}}, ?EG_MSG_TABLE_OPTS, system),
        insert_default(?DEFAULT_GAME_CATEGORIES),
        insert_default(?DEFAULT_GAME_TYPES),
        insert_default(?DEFAULT_ACCOUNTS),
        insert_default(?DEFAULT_ROLES),
        insert_default(?DEFAULT_CMDS),
        insert_default(?DEFAULT_VIEWS),
        imem_snap:suspend_snap_loop(),
        process_flag(trap_exit, true),
        {ok,#state{}}
    catch
        _Class:Reason -> {stop, {Reason,erlang:get_stacktrace()}}
    end,
    Result.

start_link() ->
    ?Info("~p starting...~n", [?MODULE]),
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], [{spawn_opt, [{fullsweep_after, 0}]}]) of
        {ok, _} = Success ->
            ?Info("~p started!~n", [?MODULE]),
            Success;
        Error ->
            ?Error("~p failed to start ~p~n", [?MODULE, Error]),
            Error
    end.

handle_call({create, GameType, MyAccountId}, _From, State) when is_binary(GameType), is_integer(MyAccountId) ->
    % Unconditionally create a game
    case read_type(GameType) of
        #egGameType{cid=Cid} ->
            GameId = rand:uniform(1844674407370955200),
            write_game(#egGame{ gid=GameId
                              , tid=GameType
                              , cid=Cid
                              , players=[MyAccountId]
                              , bots=[read_bot(MyAccountId)]
                              , ctime=eg_time()
                              }),
            {reply, GameId, State};
        _ ->
            {reply, ?NO_SUCH_GAME_TYPE, State}
    end;
handle_call({create, _GameType, MyAccountId, MyAccountId}, _From, State) ->
        {reply, ?UNIQUE_PLAYERS, State};
handle_call({create, GameType, YourAccountId, MyAccountId}, _From, State) when is_binary(GameType), is_integer(YourAccountId), is_integer(MyAccountId) ->
    % Unconditionally create a challenge (game requesting a particular co-player)
    % If proposed YourAccountId is a bot, it will automatically be accepted (Status=playing)
    case imem_meta:read(egGameType, GameType) of
        [#egGameType{cid=Cid}] ->
            % ToDo: check for existing account
            GameId = rand:uniform(1844674407370955200),
            Game = #egGame{ gid=GameId
                          , tid=GameType
                          , cid=Cid
                          , players=[MyAccountId, YourAccountId]
                          , ctime=eg_time()
                          },
            MyBot = read_bot(MyAccountId),
            case read_bot(YourAccountId) of
                undefined ->
                    write_game(Game#egGame{bots=[MyBot, undefined]}),
                    {reply, GameId, State};
                YourBot ->
                    case resume_bots(GameType, [MyBot, YourBot]) of
                        ok ->
                            start_game(prepare(Game#egGame{bots=[MyBot, YourBot], status=playing, stime=eg_time()})),
                            {reply, GameId, State};
                        Error ->
                            {reply, Error, State}
                    end
            end;
        _ ->
            {reply, ?NO_SUCH_GAME_TYPE, State}
    end;
handle_call({start, GameType, MyAccountId}, From, State) when is_binary(GameType), is_integer(MyAccountId) ->
    % Find a matching challenge (requesting my participation) in forming state
    case imem_meta:select(egGame, [ {#egGame{tid=GameType, status=forming, players=['$1', MyAccountId], _ = '_'}
                                  , [{'/=', '$1', MyAccountId}]
                                  , ['$_']}
                                  ]) of
        {[], true} ->           % forward to look for an any-player game in forming state
            handle_call({start_any, GameType, MyAccountId}, From, State);
        {Games, _} ->
            #egGame{gid=GameId, bots=Bots} = Game = lists:nth(rand:uniform(length(Games)), Games),   % pick one game at random
            case resume_bots(GameType, Bots) of
                ok ->
                    start_game(prepare(Game#egGame{status=playing, stime=eg_time()})),
                    {reply, GameId, State};
                Error ->
                    {reply, Error, State}
            end
    end;
handle_call({start_any, GameType, MyAccountId}, From, State) when is_binary(GameType), is_integer(MyAccountId) ->
    % Find a matching game (not requesting particular players) in forming state
    case imem_meta:select(egGame, [ {#egGame{tid=GameType, status=forming, players=['$1'], _ = '_'}
                                  , [{'/=', '$1', MyAccountId}]
                                  , ['$_']}
                                  ]) of
        {[], true} ->           % no invitation exists, forward to unconditional game creation
            handle_call({create, GameType, MyAccountId}, From, State);
        {Games, _} ->
            #egGame{players=[P], bots=[B]} = Game = lists:nth(rand:uniform(length(Games)), Games),   % pick one game at random
            Bots = [B, read_bot(MyAccountId)],
            case resume_bots(GameType, Bots) of
                ok ->
                    start_game(prepare(Game#egGame{players=[P, MyAccountId], bots=Bots, status=playing, stime=eg_time()})),
                    {reply, Game#egGame.gid, State};
                Error ->
                    {reply, Error, State}
            end
    end;
handle_call({start, _GameType, MyAccountId, MyAccountId}, _From, State) ->
    {reply, ?UNIQUE_PLAYERS, State};
handle_call({start, GameType, YourAccountId, MyAccountId}, From, State) when is_binary(GameType), is_integer(YourAccountId), is_integer(MyAccountId) ->
    % Find a matching game offering (specific to me) create one
    case imem_meta:select(egGame, [ {#egGame{tid=GameType, status=forming, players=[YourAccountId, MyAccountId], _ = '_'}
                                  , []
                                  , ['$_']}
                                  ]) of
        {[], true} ->
            handle_call({create, GameType, YourAccountId, MyAccountId}, From, State);
        {Games, _} ->
            Game = lists:nth(rand:uniform(length(Games)), Games),   % pick one game at random
            case resume_bots(GameType, Game#egGame.bots) of
                ok ->
                    start_game(prepare(Game#egGame{status=playing, stime=eg_time()})),
                    {reply, Game#egGame.gid, State};
                Error ->
                    {reply, Error, State}
            end
    end;
handle_call({cancel, GameId, MyAccountId}, _From, State) when is_integer(GameId), is_integer(MyAccountId) ->
    case imem_meta:read(egGame, GameId) of
        [#egGame{status=forming, players=[MyAccountId]}] ->
            imem_meta:delete(egGame, GameId),
            {reply, ok, State};
        [#egGame{status=forming, players=[MyAccountId,_]}] ->
            imem_meta:delete(egGame, GameId),
            {reply, ok, State};
        [#egGame{status=forming}] ->
            {reply, ?NOT_YOUR_GAME, State};
        [#egGame{status=Status}] ->
            S = atom_to_binary(Status, utf8),
            {reply, ?CANCEL_STATUS(S), State};
        _ ->
            {reply, ?NO_SUCH_GAME, State}
    end;
handle_call({accept, GameId, MyAccountId}, _From, State) when is_integer(GameId), is_integer(MyAccountId) ->
    case imem_meta:read(egGame, GameId) of
        [#egGame{status=forming, tid=GameType, players=[_, MyAccountId], bots=Bots} = Game] ->
            case resume_bots(GameType, Bots) of
                ok ->
                    start_game(prepare(Game#egGame{status=playing, stime=eg_time()})),
                    {reply, ok, State};
                Error ->
                    {reply, Error, State}
            end;
        [#egGame{status=forming, players=[MyAccountId, _]}] ->
            {reply, ?UNIQUE_PLAYERS, State};
        [#egGame{status=forming, players=[MyAccountId]}] ->
            {reply, ?UNIQUE_PLAYERS, State};
        [#egGame{status=forming, tid=GameType, players=[Player], bots=[Bot]} = Game] ->
            Bots = [Bot, read_bot(MyAccountId)],
            case resume_bots(GameType, Bots) of
                ok ->
                    start_game(prepare(Game#egGame{ status=playing
                                                 , players=[Player, MyAccountId]
                                                 , bots=Bots
                                                 , stime=eg_time()
                                                 }
                                     )),
                    {reply, ok, State};
                Error ->
                    {reply, Error, State}
            end;
        [#egGame{status=forming, players=[_, _]}] ->
            {reply, ?ACCEPT_MISMATCH, State};
        [#egGame{status=Status}] ->
            S = atom_to_binary(Status, utf8),
            {reply, ?ACCEPT_STATUS(S), State};
        _ ->
            {reply, ?NO_SUCH_GAME, State}
    end;
handle_call(_Request, _From, State) -> {reply, ?BAD_COMMAND, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(normal, _State) -> ?Info("~p normal stop~n", [?MODULE]);
terminate(shutdown, _State) -> ?Info("~p shutdown~n", [?MODULE]);
terminate({shutdown, _Term}, _State) -> ?Info("~p shutdown : ~p~n", [?MODULE, _Term]);
terminate(Reason, _State) -> ?Error("~p stopping unexpectedly : ~p~n", [?MODULE, Reason]).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

format_status(_Opt, [_PDict, _State]) -> ok.


% egambo_game:create(<<"tic_tac_toe">>, 2).
% egambo_game:create(<<"tic_tac_toe">>, 1, 2).
% egambo_game:start(<<"tic_tac_toe">>,  2).
% egambo_game:start(<<"tic_tac_toe">>, 1,  2).
% egambo_game:cancel(926946506377236097, 2).
% egambo_game:accept(1227778950635753473, 2).
% egambo_game:accept(1227778950635753473, 1).
% egambo_game:status(GameId, 2).
% egambo_game:play(664820677776998785, 7, $X, 2).
% egambo_game:result(664820677776998785).
% egambo_game:moves(664820677776998785).
% egambo_tictac:resume(72673005093445425).
% egambo_tictac:play(72673005093445425, 5, $X, 2).
% egambo_game:play(72673005093445425,a1).
% egambo_game:create(<<"tic_tac_toe">>, 10, 1,4).

