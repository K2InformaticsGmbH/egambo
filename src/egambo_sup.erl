-module(egambo_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), #{id => I, start => {I, start_link, []},
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
      , [ ?CHILD(egambo_game, worker)
        , ?CHILD(egambo_player_sup, supervisor)
        , ?CHILD(egambo_session_sup, supervisor)
        , ?CHILD(egambo_tictac_sup, supervisor)
        , ?CHILD(egambo_bot_sup, supervisor)
        ]
      }
    }.

