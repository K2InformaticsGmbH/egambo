-module(egambo).

% Development helper function 
-export([start/0, stop/0]).

start() ->
    {ok, _} = application:ensure_all_started(?MODULE).

stop() ->
    application:stop(?MODULE).
