-module(database).
-export([start/0, add_server/1, remove_server/1, get_servers/0]).

start() ->
    ets:new(database, [named_table, public, set]).

add_server(Server) ->
    ets:insert(database, Server).

remove_server(Server) ->
    ets:delete(process_registry, Server).

get_servers() ->
    ets:tab2list(database).
