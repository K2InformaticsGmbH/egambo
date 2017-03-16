-module(egambo_app).

-behaviour(application).

-include("egambo.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ?Info("---------------------------------------------------"),
    ?Info("STARTING EGAMBO"),
    Routes = egambo:get_routes(),
    ok = dderl:insert_routes(https, cowboy_router:compile([{'_', Routes}])),
    case egambo_sup:start_link() of
        {ok, SupRes} ->
            ?Info("EGAMBO STARTED"),
            ?Info("---------------------------------------------------"),
            {ok, SupRes};
        Other -> Other
    end.

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

