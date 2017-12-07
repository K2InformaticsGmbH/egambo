-module(egambo).

-include("egambo.hrl").

-export([
    start/0,
    stop/0,
    get_routes/0,
    start_tpcjson_listener/0,
    priv_dir/0
]).

start() ->
    {ok, _} = application:ensure_all_started(?MODULE).

stop() ->
    application:stop(?MODULE).

-spec get_routes() -> [{list(), atom(), term()}].
get_routes() ->
    %% TODO: Check why dderl is not that simple
    %% probably we need explicit trailing / support ?
    PublicDir = filename:join([priv_dir(), "public"]),
    [{"/", cowboy_static, {file, PublicDir ++ "/index.html"}},
     {"/api/[...]", egambo_resource, []},
     {"/[...]", cowboy_static, {dir, PublicDir}}].

-spec priv_dir() -> list().
priv_dir() -> priv_dir(?MODULE).

-spec priv_dir(atom()) -> list().
priv_dir(App) ->
    case code:priv_dir(App) of
        {error, bad_name} -> "priv";
        PDir -> PDir
    end.

-spec start_tpcjson_listener() -> {ok, pid()} | {error, badarg}.
start_tpcjson_listener() ->
    SSLOpts = dderl:get_ssl_options(),
    {ok, ListenPort} = application:get_env(etcpjson, port),
    {ok, Interface} = application:get_env(etcpjson, interface),
    {ok, ListenIf} = inet:getaddr(Interface, inet),
    TransOpts = [{ip, ListenIf}, {port, ListenPort}] ++ SSLOpts,
    ?Info("ssl tcpjson server listening ~s:~p ~p", [inet:ntoa(ListenIf), ListenPort, SSLOpts]),
    %% MAXACCEPTORS is defined in dderl, maybe we need our own config.
    {ok, _ListenerPid} = ranch:start_listener(?MODULE, ?MAXACCEPTORS, ranch_ssl,
        TransOpts, etcpjson_srv, [egambo_tcp_handler]),
    ok.
