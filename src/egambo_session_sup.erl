-module(egambo_session_sup).
-behaviour(supervisor).

-include("egambo.hrl").

%% API
-export([start_link/0, start_session/5, close_session/1, list_sessions/0]).

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

-spec start_session(binary(), binary(),
                    integer(), integer(), function()) -> {error, term()} | {ok, pid()}.
start_session(SessionToken, XSRFToken, MaxSessionTimeout, PingTimeout, ConnInfoFun) ->
    supervisor:start_child(?MODULE, [SessionToken, XSRFToken, MaxSessionTimeout, 
                                     PingTimeout, ConnInfoFun]).

-spec close_session(pid()) -> ok | {error, not_found | simple_one_for_one}.
close_session(SessionPid) ->
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
