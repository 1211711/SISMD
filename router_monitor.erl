-module(router_monitor).

-import(router, [startWithMonitor/3]).
-import(helper,[get_process_alias/1]).

-export([start_monitor/3]).

start_monitor(Router, MonitorName, Servers) -> 
    compile:file(helper),  
    Pid = spawn(fun() -> init(Router, Servers) end),
    io:format("ROUTER MONITOR::~p@~p:: Monitor requested for ~p@~p~n", [MonitorName, Pid, get_process_alias(Router), Router]),
    register(MonitorName, Pid),
    Pid.

init(Router, Servers) ->
    process_flag(trap_exit, true),
    loop(Router, Servers).

loop(Router, Servers) ->
    receive
        % Start Router monitoring
        {monitor, Router} ->
            io:format("ROUTER MONITOR::~p@~p:: Router ~p@~p being monitored.~n", [get_process_alias(self()), self(), get_process_alias(Router), Router]),
            link(Router),
            loop(Router, get_process_alias(Router), Servers)
    end.

loop(Router, RouterName, Servers) ->
    receive
        % Restart Router when it goes down
        {'EXIT', Router, Reason} ->
            io:format("ROUTER MONITOR::~p@~p::EXIT:: Router ~p@~p is down due to: ~p~n", [get_process_alias(self()), self(), RouterName, Router, Reason]),
            NewRouter = startWithMonitor(RouterName, self(), Servers),
            loop(NewRouter, Servers);
        % Refresh servers
        {refreshServer, ServerName, OldServer, NewServer} ->
            io:format("ROUTER MONITOR::~p@~p:: Refresh server ~p from ~p to ~p~n", [get_process_alias(self()), self(), ServerName, OldServer, NewServer]),
            NewServers = lists:keyreplace(ServerName, 1, Servers, {ServerName, NewServer}),
            io:format("ROUTER MONITOR::~p@~p:: SERVERS: ~p~n", [get_process_alias(self()), self(), NewServers]),
            loop(Router, RouterName, NewServers);
        % Add server
        {add_server, ServerName, Server} ->
            NewServers = [{ServerName, Server} | lists:keydelete(ServerName, 1, Servers)],
            io:format("ROUTER MONITOR::~p@~p:: SERVERS: ~p~n", [get_process_alias(self()), self(), NewServers]),
            loop(Router, RouterName, NewServers)
    end.