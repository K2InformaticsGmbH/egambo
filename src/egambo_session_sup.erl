-module(egambo_session_sup).
-behaviour(supervisor).

-include("egambo.hrl").

%% API
-export([
    start_link/0,
    start_session/2,
    start_session/3,
    close_session/1,
    list_sessions/0
]).

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

-spec start_session(binary(), fun()) -> {error, term()} | {ok, pid()}.
start_session(SessionId, MsgFun) ->
    %% From tcp there is no xsrf token.
    start_session(SessionId, <<>>, MsgFun).

-spec start_session(binary(), binary(), fun()) -> {error, term()} | {ok, pid()}.
start_session(SessionId, XSRFToken, MsgFun) ->
    supervisor:start_child(?MODULE, [SessionId, XSRFToken, MsgFun]).

-spec close_session(pid() | binary()) -> ok | {error, not_found | simple_one_for_one}.
close_session(SessionId) when is_binary(SessionId) ->
    case global:whereis_name(SessionId) of
        undefined -> {error, not_found};
        Pid -> supervisor:terminate_child(?MODULE, Pid)
    end;
close_session(SessionPid) when is_pid(SessionPid) ->
    supervisor:terminate_child(?MODULE, SessionPid).

-spec list_sessions() -> [pid()].
list_sessions() ->
    [Pid || {undefined, Pid, worker, _} <- supervisor:which_children(?MODULE)].

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    SupFlags = {simple_one_for_one, 5, 10},
    {ok, {SupFlags, [?CHILD(egambo_session)]}}.
