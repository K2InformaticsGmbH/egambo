-module(egambo_session_sup).
-behaviour(supervisor).

-include("egambo.hrl").

%% API
-export([start_link/0, start_session/1, close_session/1, list_sessions/0]).

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

-spec start_session(binary()) -> {error, term()} | {ok, pid()}.
start_session(SessionId) ->
    supervisor:start_child(?MODULE, [SessionId]).

-spec close_session(pid() | binary()) -> ok | {error, not_found | simple_one_for_one}.
close_session(SessionId) when is_binary(SessionId) ->
    case global:whereis_name(SessionId) of
        undefined -> {error, not_found};
        Pid -> supervisor:terminate_child(?MODULE, Pid)
    end;
close_session(SessionPid) when is_pid(SessionPid) ->
    supervisor:terminate_child(?MODULE, SessionPid).

-spec list_sessions() -> [binary()]. %%TODO: What do we return here, tokens ?
list_sessions() ->
    supervisor:which_children(?MODULE).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    SupFlags = {simple_one_for_one, 5, 10},
    {ok, {SupFlags, [?CHILD(egambo_session)]}}.
