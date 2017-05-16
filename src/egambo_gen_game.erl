-module(egambo_gen_game).

-include("egambo_game.hrl").

-callback create(GameType::integer(), Opts::ddOptions(), MyAccountId::ddEntityId()) -> egGameId() | egGameError().
-callback create(GameType::integer(), YourAccountId::ddEntityId(), Opts::ddOptions(), MyAccountId::ddEntityId()) -> egGameId() | egGameError().
-callback start(GameType::integer(), Opts::ddOptions(), MyAccountId::ddEntityId()) -> egGameId() | egGameError().
-callback start(GameType::integer(), YourAccountId::ddEntityId(), Opts::ddOptions(), MyAccountId::ddEntityId()) -> egGameId() | egGameError().
-callback cancel(GameId::egGameId(), Opts::ddOptions(), MyAccountId::ddEntityId()) -> ok | egGameError().
-callback accept(GameId::egGameId(), Opts::ddOptions(), MyAccountId::ddEntityId()) -> ok | egGameError().
-callback play(GameId::egGameId(), Move::egGameMove(), Opts::ddOptions(), MyAccountId::ddEntityId()) -> ok | egGameError().
-callback status(GameId::egGameId(), Opts::ddOptions(), MyAccountId::ddEntityId()) -> egGameResult() | egGameError().

