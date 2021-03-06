-ifndef(_EGAMBO_TICTAC_HRL_).
-define(_EGAMBO_TICTAC_HRL_, true).

-include("egambo_game.hrl").    % import game managing structures 

-define(OBSTACLE, $$).
-define(JOKER, $*).
-define(AVAILABLE, 32).         		% space
-define(MAX_DIMENSION, 19).     		% large GO board
-define(COLS, "abcdefghijklmnopqrs").	% large GO board
-define(ROWS, "ABCDEFGHIJKLMNOPQRS").	% large GO board

-define(INVALID_CELL, {error, invalid_cell}).
-define(INVALID_BOT, {error, invalid_bot}).
-define(BOT_NOT_PLAYING, {error, bot_not_playing}).
-define(ALREADY_OCCUPIED, {error, already_occupied}).
-define(INVALID_ALIAS_PARAMETER, {error, invalid_alias_parameter}).
-define(INVALID_WIDTH_PARAMETER, {error, invalid_width_parameter}).
-define(INVALID_HEIGHT_PARAMETER, {error, invalid_height_parameter}).
-define(INVALID_RUN_PARAMETER, {error, invalid_run_parameter}).
-define(INVALID_BOARD_PARAMETER, {error, invalid_board_parameter}).
-define(INVALID_PARAMETER_CONFIG, {error, invalid_parameter_config}).
-define(MISSING_WIN_MODULE, {error, missing_win_module}).
-define(MISSING_WIN_FUNCTION, {error, missing_win_function}).

-define(EG_SEC(__T),egambo_game:eg_time_to_sec(__T)).
-define(EG_MSEC(__T),egambo_game:eg_time_to_msec(__T)).
-define(EG_USEC(__T),egambo_game:eg_time_to_usec(__T)).

-type egWinId()             :: undefined | module().  % module which implements win(Board,Player) function

-type egTicTacParams() ::  #{ width => integer()
                            , height => integer()
                            , run => integer()
                            , gravity => boolean()
                            , periodic => boolean()
                            , obstacles => [integer()] | integer()
                            , jokers => [integer()] | integer()
                            , aliases => [integer()]
                            }.

-endif.
