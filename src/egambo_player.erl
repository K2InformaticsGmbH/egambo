-module(egambo_player).

-export([
    new_game/5,
    accept_game/2,
    cancel_game/2,
    play/3
]).

% TODO: We should call from here the egambo_game...
-spec new_game(fun(), binary(), map(), binary(), integer() | unlimited) -> ok.
new_game(ReplyFun, Game, BoardParameters, Category, TLimit) ->
    %egambo_tic_tac:start(3,3,3,0,0,"X","O",undefined,false,false).
    ok.

-spec accept_game(fun(), integer()) -> ok.
accept_game(ReplyFun, GameId) ->
    ok.


-spec cancel_game(fun(), integer()) -> ok.
cancel_game(ReplyFun, GameId) ->
    ok.

-spec play(fun(), integer(), integer()) -> ok.
play(ReplyFun, GameId, Pos) -> % pos starts from 0
    ok. 
