-module(egambo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    egambo_sup:start_link().

stop(_State) ->
    ok.

%% ==================
%% TESTS
%% ==================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    ok = application:start(egambo),
    ?assertNot(undefined == whereis(egambo_sup)).

-endif.

