-module(egambo_gen_game).

-include("egambo_game.hrl").

-export([]).

-callback play(GameId::egGameId(), Move::egGameMove(), MyAccountId::ddEntityId()) -> ok | egGameError().
-callback result(GameId::egGameId() | #egGame{}) -> egGameResult() | egGameError().
-callback moves(GameId::egGameId()| #egGame{}) -> egGameMoves() | egGameError().

