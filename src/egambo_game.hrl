-ifndef(_EGAMBO_GAME_HRL_).
-define(_EGAMBO_GAME_HRL_, true).

-include_lib("imem/include/imem.hrl").
-include_lib("imem/include/imem_meta.hrl").

-type egGameTypeId()            :: binary().                
-type egGameCategoryId()        :: binary().                
-type egGameId()                :: integer().               % random integer
-type egGameMove()              :: integer() | atom().      % e.g. board index, maybe parameterized commands later
-type egBotMove()               :: {ok, integer(), binary()} | egGameError().  % e.g. board index / move command and NewBoard
-type egBotMoveCmd()            :: {ok, integer(), binary()} | egGameError().  % e.g. board index / move command and NewBoard
-type egAccountId()             :: ddEntityId().            % imem AccountId
-type egAlias()                 :: integer().               % e.g. ascii of letters X and O
-type egScore()                 :: float().                 % 1.0=win, 0.0=tie, -1.0=lose (interpolations possible)
-type egGameMsgType()           :: create | cancel | play | close.   
-type egGameMsg()               :: term().
-type egTime()                  :: ddTimeUID().
-type egBotId()                 :: undefined | module().
-type egBotStatus()             :: undefined | starting | learning | playing | stopped.
-type egEngine()                :: module().
-type egGameResult()            :: #{id=>egGameId(), etime=> egTime(), status=>egGameStatus(), board=>binary(), movers=>[egAccountId()], aliases=>[egAlias()], scores=>[egScore()]}.
-type egGameMoves()             :: #{id=>egGameId(), etime=> egTime(), status=>egGameStatus(), space=>binary(), moves=>[term()]}.
-type egGameStatus()            :: forming | playing | paused | finished | aborted.
-type egGameError()             :: {error, atom()} | {error, binary()} | {error, {atom(), term()}} | {error, {binary(), term()}}.

-define(egGameNotImplemented,   {error, not_implemented}).

-define(ENGINE_ID(__GameId), {egambo, __GameId}).             % name of game engine instance
-define(ENGINE_GID(__GameId), {global, {egambo, __GameId}}).  % global name of game engine instance
-define(BOT_ID(__BotId, __GameTypeId), {egambo_bot, __BotId, __GameTypeId}).   % name of bot instance
-define(BOT_GID(__BotId, __GameTypeId), {global, {egambo_bot, __BotId, __GameTypeId}}).   % global name of bot instance

-record(egGameCategory, { cid      = <<>>  :: egGameCategoryId()    % game category id
                        , cname    = <<>>  :: binary()              % game category name
                        , info     = <<>>  :: binary()              % game category description
                        }).
-define(egGameCategory, [ binstr    % cid
                        , binstr    % cname
                        , binstr    % info
                        ]).
% rd(egGameCategory, {cid=, cname=, info=}).

-record(egGameType, { tid      = <<>>  :: egGameTypeId()      % game type id
                    , tname    = <<>>  :: binary()            % game type name
                    , cid      = <<>>  :: egGameCategoryId()  % game category id
                    , engine           :: module()            % module implementing the egambo_gen_game behaviour
                    , players  = 0     :: integer()           % number of players
                    , params   = #{}   :: map()               % game internal setup parameters (board size, etc.) 
                    , setup    = #{}   :: map()               % manager setup options (eg. deprecated, paring rules) 
                    , level    = 0.0   :: term()              % game difficulty
                    , info     = <<>>  :: binary()            % game description
                    }).
-define(egGameType, [ binstr    % tid
                    , binstr    % tname
                    , binstr    % cid
                    , atom      % engine
                    , integer   % players
                    , term      % params
                    , term      % setup
                    , term      % level
                    , binstr    % info
                    ]).
% rd( egGameType, {tid=, tname=, cid=, engine=, players=, params=, setup=, level=, info=}).

-record(egGame, { gid      = 0              :: egGameId()          % unique game id using ramdom integer
                , tid      = <<>>           :: egGameTypeId()      % game type id
                , cid      = <<>>           :: egGameCategoryId()  % game category id
                , players  = []             :: [egAccountId()]     % hd(players) is owner (proposer) of the game 
                , bots     = []             :: [egBotId()]         % internal bot player modules (undefined for external players)
                , ctime    = undefined      :: egTime()            % game create time 
                , ialiases = []             :: [egAlias()]         % initial integer aliases (codes) of players matching initial movers order
                , imovers  = []             :: [egAccountId()]     % initial player enumeration in move sequence order
                , space    = <<>>           :: term()              % initialized game space (initial board, needed to reconstruct game)
                , stime    = undefined      :: egTime()            % game start time (forming complete time)
                , etime    = undefined      :: egTime()            % last status change time (so far), game end time (eventually)
                , status   = forming        :: egGameStatus()      % current game (management) status
                , board    = <<>>           :: term()              % current board (game state) before next move
                , nmovers  = []             :: [egAccountId()]     % next player AccountId enumeration for coming moves
                , naliases = []             :: [egAlias()]         % next player aliases (codes) for next moves
                , nscores  = []             :: [egScore()]         % player scores in next mover order (not player order)
                , moves    = []             :: [term()]            % reversed list of moves by mover0, mover1, mover0, ... 
                }).
-define(egGame, [ integer     % gid
                , binstr      % tid
                , binstr      % cid
                , list        % players
                , list        % bots
                , timestamp   % ctime
                , list        % ialiases
                , list        % imovers
                , binstr      % space
                , timestamp   % stime
                , timestamp   % etime
                , atom        % status
                , binstr      % board
                , list        % nmovers
                , list        % naliases
                , list        % nscores
                , list        % moves
                ]).
% rd(egGame,{gid=, tid=, cid=, players=, bots=, ctime=, ialiases=, imovers=, space=, stime=, etime=, status=, board=, nmovers=, naliases=, nscores=, moves=}).

-record(egGameMsg,  { time      = undefined         :: ddTimestamp() 
                    , gid       = 0                 :: egGameId()       
                    , msgtype   = create            :: egGameMsgType()  
                    , message   = undefined         :: egGameMsg() 
                    }).
-define(egGameMsg,  [ timestamp 
                    , integer
                    , atom
                    , term
                    ]).
% rd(egGameCategory, {time=, gid=, msgtype=, message=}).

-endif.
