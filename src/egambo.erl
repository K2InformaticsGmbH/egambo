-module(egambo).

-export([start/0
        ,stop/0
        ,get_routes/0]).

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
     {"/[...]", cowboy_static, {dir, PublicDir}}].

-spec priv_dir() -> list().
priv_dir() -> priv_dir(?MODULE).

-spec priv_dir(atom()) -> list().
priv_dir(App) ->
    case code:priv_dir(App) of
        {error, bad_name} -> "priv";
        PDir -> PDir
    end.
