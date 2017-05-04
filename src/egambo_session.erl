-module(egambo_session).

%% Placeholder module for when we have registered players.
%-behaviour(gen_server).

-include("egambo.hrl").

-export([
    request/2
]).

-spec request(map(), fun()) -> pid().
request(#{<<"action">> := Action} = ReqArgs, ReplyFun) ->
    % This should go to the gen_server once we have sessions.
    spawn(fun() ->
        process_request(Action, ReplyFun, ReqArgs)
    end).

-spec process_request(binary(), fun(), map()) -> ok.
process_request(<<"create_challenge">>, ReplyFun, #{<<"game">> := Game,
    <<"category">> := Category, <<"board_parameters">> := BoardParameters,
    <<"time_limit">> := TLimit}) ->
    egambo_player:new_game(ReplyFun, Game, BoardParameters, Category, TLimit);
process_request(<<"accept_challenge">>, #{<<"game_id">> := GameId}, ReplyFun) ->
    egambo_player:accept_challenge(ReplyFun, GameId);
process_request(<<"cancel_challenge">>, #{<<"game_id">> := GameId}, ReplyFun) ->
    egambo_player:cancel_challenge(ReplyFun, GameId);
process_request(<<"play">>, #{<<"game_id">> := GameId, <<"position">> := Pos}, ReplyFun) ->
    egambo_player:play(ReplyFun, GameId, Pos);
process_request(_Action, _Args, ReplyFun) ->
    ReplyFun(#{error => <<"unsupported request">>}),
    ok.