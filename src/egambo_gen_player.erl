-module(egambo_gen_player).

-include("egambo_game.hrl").

-export([]).

-callback create(GameType::egGameTypeId(), Opts::ddOptions(), MyAccountId::ddEntityId()) ->  egGameId() | egGameError().
-callback create(GameType::egGameTypeId(), YourAccountId::ddEntityId(), Opts::ddOptions(), MyAccountId::ddEntityId()) -> egGameId() | egGameError().

-callback start(GameType::egGameTypeId(), Opts::ddOptions(), MyAccountId::ddEntityId()) ->  egGameId() | egGameError().
-callback start(GameType::egGameTypeId(), YourAccountId::ddEntityId(), Opts::ddOptions(), MyAccountId::ddEntityId()) -> egGameId() | egGameError().

-callback cancel(GameId::egGameId(), Opts::ddOptions(), MyAccountId::ddEntityId()) -> ok | egGameError().

-callback accept(GameId::egGameId(), Opts::ddOptions(), MyAccountId::ddEntityId()) -> ok | egGameError().

-callback play(GameId::egGameId(), Move::egGameMove(), Opts::ddOptions(), MyAccountId::ddEntityId()) -> ok | egGameError().
-callback forfeit(GameId::egGameId(), Opts::ddOptions(), MyAccountId::ddEntityId()) -> ok | egGameError().
-callback status(GameId::egGameId(), Opts::ddOptions(), MyAccountId::ddEntityId()) -> egGameResult() | egGameError().

-callback handle_move(GameId::egGameId(), GameResult::egGameResult(), Opts::ddOptions()) ->  ok.
