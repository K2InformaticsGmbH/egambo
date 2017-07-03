-module(egambo_tictac_sup).
-behaviour(supervisor).

-include("egambo_game.hrl").

%% API
-export([start_link/0, start_game/1, stop_game/1, list_games/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I), {I, {I, start_link, []}, temporary, 1000, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================
-spec start_link() -> ignore | {error, term()} | {ok, pid()}.
start_link() ->
    ?Info("~p starting...~n", [?MODULE]),
    case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
        {ok, _} = Success ->
            ?Info("~p started!~n", [?MODULE]),
            Success;
        Error ->
            ?Error("~p failed to start ~p~n", [?MODULE, Error]),
            Error
    end.

-spec start_game(egGameId()) -> {ok, pid()} | {error, {already_starte, pid()}}.
start_game(GameId) ->
    supervisor:start_child(?MODULE, [GameId]).

-spec stop_game(pid()) -> ok | {error, not_found | simple_one_for_one}.
stop_game(GamePid) ->
    supervisor:terminate_child(?MODULE, GamePid).

-spec list_games() -> [egGameId()]. %%TODO: What do we return here, tokens ?
list_games() ->
    supervisor:which_children(?MODULE).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	SupFlags = {simple_one_for_one, 5, 10},
    {ok, {SupFlags, [?CHILD(egambo_tictac)]}}.
