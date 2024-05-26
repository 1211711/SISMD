-module(router_monitor).

-import(router, [startWithMonitor/4]).
-import(helper,[get_process_alias/1]).

-export([start_monitor/4]).

start_monitor(Router, MonitorName, Servers, ServerMonitors) -> 
    compile:file(helper),  
    Pid = spawn(fun() -> init(Router, Servers, ServerMonitors) end),
    io:format("ROUTER MONITOR::~p@~p:: Monitor requested for ~p@~p~n", [MonitorName, Pid, get_process_alias(Router), Router]),
    register(MonitorName, Pid),
    Pid.

init(Router, Servers, ServerMonitors) ->
    process_flag(trap_exit, true),
    loop(Router, Servers, ServerMonitors).

loop(Router, Servers, ServerMonitors) ->
    receive
        % Start Router monitoring
        {monitor, Router} ->
            io:format("ROUTER MONITOR::~p@~p:: Router ~p@~p being monitored.~n", [get_process_alias(self()), self(), get_process_alias(Router), Router]),
            link(Router),
            loop(Router, get_process_alias(Router), Servers, ServerMonitors)
    end.

loop(Router, RouterName, Servers, ServerMonitors) ->
    receive
        % Restart Router when it goes down
        {'EXIT', Router, Reason} ->
            io:format("ROUTER MONITOR::~p@~p::EXIT:: Router ~p@~p is down due to: ~p~n", [get_process_alias(self()), self(), RouterName, Router, Reason]),
            NewRouter = startWithMonitor(RouterName, self(), Servers, ServerMonitors),
            loop(NewRouter, Servers, ServerMonitors);
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
            loop(Router, RouterName, NewServers, ServerMonitors);
        % Add server
        {add_server_monitor, ServerName, ServerMonitor} ->
            NewServerMonitors = [{ServerName, ServerMonitor} | lists:keydelete(ServerName, 1, ServerMonitors)],
            io:format("ROUTER MONITOR::~p@~p:: SERVER MONITORS: ~p~n", [get_process_alias(self()), self(), NewServerMonitors]),
            loop(Router, RouterName, Servers, NewServerMonitors)
    end.