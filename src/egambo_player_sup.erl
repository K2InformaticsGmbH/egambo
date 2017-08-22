-module(egambo_player_sup).
-behaviour(supervisor).

-include("egambo.hrl").

%% API
-export([start_link/0, start_player/1, close_player/1, list_players/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I), {I, {I, start_link, []}, temporary, 5000, worker, [I]}).

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

-spec start_player(egAccountId()) -> {error, term()} | {ok, pid()}.
start_player(PlayerId) ->
    supervisor:start_child(?MODULE, [PlayerId]).

-spec close_player(egAccountId()) -> ok | {error, not_found | simple_one_for_one}.
close_player(PlayerPid) when is_pid(PlayerPid) ->
    supervisor:terminate_child(?MODULE, PlayerPid);
close_player(PlayerId) ->
    {global, Name} = ?PLAYER_GID(PlayerId),
    case global:whereis_name(Name) of
        undefined -> {error, not_found};
        Pid -> supervisor:terminate_child(?MODULE, Pid)
    end.

-spec list_players() -> [pid()].
list_players() ->
    [Pid || {undefined, Pid, worker, _} <- supervisor:which_children(?MODULE)].

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    SupFlags = {simple_one_for_one, 5, 10},
    {ok, {SupFlags, [?CHILD(egambo_player)]}}.
