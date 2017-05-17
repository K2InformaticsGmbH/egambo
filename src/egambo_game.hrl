-ifndef(_EGAMBO_GAME_HRL_).
-define(_EGAMBO_GAME_HRL_, true).

-include_lib("imem/include/imem.hrl").
-include_lib("imem/include/imem_meta.hrl").

-type egGameTypeId()            :: binary().                
-type egGameCategoryId()        :: binary().                
-type egGameId()                :: integer().               % random integer
-type egGameMove()              :: integer() | atom().      % e.g. board index, maybe parameterized commands later
-type egPlayerScore()           :: float().                 % 1.0=win, 0.0=tie, -1.0=lose (interpolations possible)
-type egGameResult()            :: #{id=>egGameId(), status=>egGameStatus(), board=>binary(), movers=>[integer()], aliases=>[integer()], scores=>[egPlayerScore()]}.
-type egGameStatus()            :: forming | playing | paused | finished | aborted.
-type egGameError()             :: {error, atom()} | {error, binary()} | {error, {atom(), term()}} | {error, {binary(), term()}}.

-define(egGameNotImplemented,   {error, not_implemented}).


-record(egGameCategory, { cid      = <<>>  :: egGameCategoryId()    % game category id
                        , cname    = <<>>  :: binary()              % game category name
                        , info     = <<>>  :: binary()              % game category description
                        }).
-define(egGameCategory, [ binstr    % cid
                        , binstr    % cname
                        , binstr    % info
                        ]).
% rd(egGameCategory, {cid= <<>>, cname= <<>>, info= <<>>}).

-record(egGameType, { tid      = <<>>  :: egGameTypeId()      % game type id
                    , tname    = <<>>  :: binary()            % game type name
                    , cid      = <<>>  :: egGameCategoryId()  % game category id
                    , engine   = egambo_tictac :: atom()      % module implementing the egambo_gen_game behaviour
                    , players  = 2     :: integer()           % number of players
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
% rd( egGameType, {tid= <<>>, tname= <<>>, cid= <<>>, engine= egambo_tictac, players=2, params=#{}, setup=#{}, level=0.0, info= <<>>}).

-record(egGame, { gid      = 0              :: egGameId()          % unique game id using ramdom integer
                , tid      = <<>>           :: egGameTypeId()      % game type id
                , cid      = <<>>           :: egGameCategoryId()  % game category id
                , players  = []             :: [ddEntityId()]      % hd(players) is owner (proposer) of the game 
                , ctime    = undefined      :: ddTimeUID()         % game create time 
                , imovers  = []             :: [ddEntityId()]      % initial player enumeration in move sequence order
                , ialiases = []             :: [integer()]         % initial integer aliases (codes) of players matching initial movers order
                , preset   = <<>>           :: binary()            % initialized game state (needed to reconstruct game)
                , stime    = undefined      :: ddTimeUID()         % game start time (forming complete time)
                , etime    = undefined      :: ddTimestamp()       % last status change time (so far), game end time (eventually)
                , status   = forming        :: egGameStatus()      % current game (management) status
                , board    = <<>>           :: binary()            % current board (game state) before next move
                , nmovers  = []             :: [ddEntityId()]      % next player AccountId enumeration for coming moves
                , naliases = []             :: [integer()]         % next player aliases (codes) for next moves
                , nscores  = []             :: [float()]           % player scores in next mover order (not player order)
                , moves    = []             :: [term()]            % reversed list of moves by mover0, mover1, mover0, ... 
                }).
-define(egGame, [ timestamp   % gid
                , binstr      % tid
                , binstr      % cid
                , list        % players
                , timestamp   % ctime
                , list        % imovers
                , list        % ialiases
                , binstr      % preset
                , timestamp   % stime
                , timestamp   % etime
                , atom        % status
                , binstr      % board
                , list        % nmovers
                , list        % naliases
                , list        % nscores
                , list        % moves
                ]).
% rd(egGame,{gid=0, tid= <<>>, cid= <<>>, players=[], imovers=[], ialiases=[$X, $O], preset= <<>>, stime, etime, status=forming, board= <<>>, nmovers=[], naliases=[], nscores=[], moves=[]}).

-endif.
