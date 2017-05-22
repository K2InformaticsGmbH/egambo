-module(egambo_gen_engine).

-include("egambo_game.hrl").

-export([]).

-callback prepare(GameType::#egGameType{}, Game::#egGame{}) -> InitializedGame::#egGame{} | egGameError(). % prepare an initial game state record
-callback resume(Game::egGameId()) -> ok | egGameError(). 	% startup the engine with given game id
-callback stop(GameId::egGameId()) -> ok | egGameError().	% stop the engine

