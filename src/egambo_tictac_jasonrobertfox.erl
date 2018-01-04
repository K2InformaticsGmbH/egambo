-module(egambo_tictac_jasonrobertfox).

-include("egambo_tictac.hrl").  % import tictac game definitions 

-behavior(gen_server).          % callbacks provided by (bot) players
%-behavior(egambo_gen_player).   % callbacks provided by (bot) players

-record(state,  { bid        :: egBotId()       % bot id (= module name)
                , tid        :: egGameTypeId()  % game type id
                , engine     :: egEngine()      % game engine (rule engine)
                , width      :: integer()       % board width >= 3
                , height     :: integer()       % board height >= 3
                , run        :: integer()       % sucess run length
                , gravity    :: boolean()       % do moves fall towards higher row numbers
                , periodic   :: boolean()       % unbounded repeating board
                , winmod     :: egWinId()       % win function module
                , players    :: integer()       % number of players 
                , status     :: egBotStatus()   % current bot status (e.g. learning / playing)
                }).

% gen_server behavior callback exports

-export([ start_link/1
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

% egambo_gen_engine behavior callbacks, used by eg_game (game manager)
-export([ resume/1      % instruct supervisor to start the bot process and resume the game
        , stop/1        % instruct supervisor to stop the bot process (no attempt to save the state)
        , game_types/1  % return list of supported game types for given game engine (or atom 'all') 
        ]).

% debugging API
-export([ state/1
        ]).

game_types(egambo_tictac) -> all;
game_types(_) -> [].

init([GameTypeId]) ->
    Result = try
        #egGameType{engine=Engine, players=Players, params=Params} = egambo_game:read_type(GameTypeId),
        Width=maps:get(width, Params),
        Height=maps:get(height, Params),
        Run=maps:get(run, Params),
        Gravity=maps:get(gravity, Params),
        Periodic=maps:get(periodic, Params),           
        State = #state{ bid=?MODULE 
                      , tid=GameTypeId
                      , engine=Engine
                      , width=Width
                      , height=Height
                      , run=Run
                      , gravity=Gravity
                      , periodic=Periodic
                      , winmod=egambo_tictac:win_module(Width, Height, Run, Periodic)
                      , players=Players
                      , status=playing
                      },
        process_flag(trap_exit, true),
        {ok, State}
    catch
        _Class:Reason -> {stop, {Reason,erlang:get_stacktrace()}} 
    end,
    Result.

start_link(GameTypeId)  ->
    gen_server:start_link(?BOT_GID(?MODULE, GameTypeId), ?MODULE, [GameTypeId], []).

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_cast({play_bot_req, GameId, _, [Player|_]}, #state{status=Status} = State) when Status /= playing ->
    play_bot_resp(GameId, Player, ?BOT_NOT_PLAYING),
    {noreply, State};
handle_cast({play_bot_req, GameId, Board, Aliases}, State) -> 
    case next(Board, hd(Aliases)) of
        {ok, NewBoard} ->
            play_bot_resp(GameId, hd(Aliases), {ok, boards2idx(Board, NewBoard), NewBoard});
        Error ->
            play_bot_resp(GameId, hd(Aliases), Error)
    end,
    {noreply, State};
handle_cast(Request, State) -> 
    ?Info("Unsolicited handle_cast in ~p : ~p",[?MODULE, Request]),
    {noreply, State}.

handle_info(Reqest, State) when element(1, Reqest) == notify_bot_req ->
    % ignore game notifications 
    {noreply, State};
handle_info(Request, State) -> 
    ?Info("Unsolicited handle_info in ~p : ~p",[?MODULE, Request]),
    {noreply, State}.

handle_call(state, _From, State) ->
    {reply, State, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

-spec play_bot_resp(egGameId(), egAlias(), egBotMove() ) -> ok.
play_bot_resp(GameId, Player, BotMove) -> 
    send_to_engine(GameId, {play_bot_resp, Player, BotMove}).

-spec send_to_engine(egGameId(), egBotMoveCmd()) -> ok.
send_to_engine(GameId, Message) ->
    try 
        global:send(?ENGINE_ID(GameId), Message),
        ok
    catch 
        exit:{badarg, {_, _}} -> ?Error("GameEngine ~p unreachable. Dropped message: ~p",[GameId, Message])
    end.

-spec resume(egGameTypeId()) -> ok | egGameError().
resume(GameTypeId) ->
    ChildSpec = { ?BOT_ID(?MODULE, GameTypeId)                  % ChildId
                , {?MODULE, start_link, [GameTypeId]}           % {M,F,A}
                , permanent                                     % do restart automatically
                , 1000                                          % Shutdown timeout
                , worker                                        % Type
                , [?MODULE]                                     % Modules
                },
    case supervisor:start_child(egambo_bot_sup, ChildSpec) of
        {ok,_} ->                       ok;
        {ok,_,_} ->                     ok;
        {error, already_present} ->     ok;
        {error,{already_started,_}} ->  ok;
        Error ->                        Error
    end.

-spec stop(egBotId()) -> ok | egGameError().
stop(GameTypeId) ->
    supervisor:terminate_child(egambo_bot_sup, ?BOT_ID(?MODULE, GameTypeId)),
    supervisor:delete_child(egambo_bot_sup, ?BOT_ID(?MODULE, GameTypeId)).

state(GameTypeId) ->
    gen_server:call(?BOT_GID(?MODULE, GameTypeId), state). 

board2map(<<TL,TC,TR,ML,MC,MR,BL,BC,BR>>) ->
    [#{id => <<"top-left">>,       value => piece2bin(TL)},
     #{id => <<"top-center">>,     value => piece2bin(TC)},
     #{id => <<"top-right">>,      value => piece2bin(TR)},
     #{id => <<"middle-left">>,    value => piece2bin(ML)},
     #{id => <<"middle-center">>,  value => piece2bin(MC)},
     #{id => <<"middle-right">>,   value => piece2bin(MR)},
     #{id => <<"bottom-left">>,    value => piece2bin(BL)},
     #{id => <<"bottom-center">>,  value => piece2bin(BC)},
     #{id => <<"bottom-right">>,   value => piece2bin(BR)}].

piece2bin($ ) -> <<"">>;
piece2bin($X) -> <<"x">>;
piece2bin($O) -> <<"o">>.

mapslist2map(M) -> mapslist2map(M, #{}).
mapslist2map([], M) -> M;
mapslist2map([#{<<"id">>:=I,<<"value">>:=V}|R], M) -> mapslist2map(R, M#{I => V}).

map2board(MapList) ->
    #{<<"top-left">>        := TL,
      <<"top-center">>      := TC,
      <<"top-right">>       := TR,
      <<"middle-left">>     := ML,
      <<"middle-center">>   := MC,
      <<"middle-right">>    := MR,
      <<"bottom-left">>     := BL,
      <<"bottom-center">>   := BC,
      <<"bottom-right">>    := BR} = mapslist2map(MapList),
    <<(bin2piece(TL)), (bin2piece(TC)), (bin2piece(TR)),
      (bin2piece(ML)), (bin2piece(MC)), (bin2piece(MR)),
      (bin2piece(BL)), (bin2piece(BC)), (bin2piece(BR))>>.

bin2piece(<<"">> ) -> $ ;
bin2piece(<<"x">>) -> $X;
bin2piece(<<"o">>) -> $O.

boards2idx(<<OTL,OTC,OTR,OML,OMC,OMR,OBL,OBC,OBR>>,
           <<NTL,NTC,NTR,NML,NMC,NMR,NBL,NBC,NBR>>) ->
    if OTL == $ andalso NTL /= $ -> 0;
       OTC == $ andalso NTC /= $ -> 1;
       OTR == $ andalso NTR /= $ -> 2;
       OML == $ andalso NML /= $ -> 3; 
       OMC == $ andalso NMC /= $ -> 4;
       OMR == $ andalso NMR /= $ -> 5;
       OBL == $ andalso NBL /= $ -> 6; 
       OBC == $ andalso NBC /= $ -> 7;
       OBR == $ andalso NBR /= $ -> 8;
       true -> -1
    end.

-define(URL, "http://perfecttictactoe.herokuapp.com/api/v2/play").
next(Board, $O) -> next(Board, <<"o">>, <<"x">>);
next(Board, $X) -> next(Board, <<"x">>, <<"o">>).
next(Board, PP, OP) ->    
    Body = jsx:encode(#{player_piece => PP, opponent_piece => OP,
                        board => board2map(Board)}),
    case catch httpc:request(
                 post,
                 {?URL, [], "application/json;charset=UTF-8", Body},
                 [{url_encode,false}],
                 [{full_result, false}, {body_format, binary}]) of
        {ok, {200, Resp}} ->
            #{<<"data">> :=
              #{<<"board">> := NewBoard,
                <<"opponent_piece">> := _OponentPiece,
                <<"player_piece">> := _PlayerPiece,
                <<"status">> := _WinOrLoose},
              <<"status">> := _Status} = jsx:decode(Resp, [return_maps]),
            {ok, map2board(NewBoard)};
        {ok, {Code, Resp}}  -> {error, {Code, Resp}};
        {error, Error}      -> {error, Error};
        Error               -> {error, Error}
    end.
