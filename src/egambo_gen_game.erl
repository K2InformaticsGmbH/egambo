-module(egambo_gen_game).

-include("egambo_game.hrl").

-export([]).

-callback preset(GameType::#egGameType{}, Game::#egGame{}, Opts::ddOptions()) -> InitializedGame::#egGame{} | egGameError().
-callback play(GameId::egGameId(), Move::egGameMove(), Opts::ddOptions(), MyAccountId::ddEntityId()) -> ok | egGameError().
-callback forfeit(GameId::egGameId(), Opts::ddOptions(), MyAccountId::ddEntityId()) -> ok | egGameError().
-callback status(GameId::egGameId(), Opts::ddOptions(), MyAccountId::ddEntityId()) -> egGameResult() | egGameError().
-callback stop() -> ok | egGameError().

