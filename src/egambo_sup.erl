-module(egambo_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), #{id => I, start => {I, start_link, Args},
                                restart => permanent, shutdown => 5000,
                                type => Type, modules => [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok
    , { {one_for_one, 5, 10}
      , [
    	?CHILD(egambo_game, worker, [])
        ]
      }
    }.

