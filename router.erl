-module(router).

-import(router_monitor,[start_monitor/2]).
-import(helper,[get_process_alias/1]).
-import(database,[start/0, add_server/1, remove_server/1, get_servers/0]).

-export([start/2, startWithMonitor/2]).

defaultStart(Router, MonitorName, Servers) ->
    compile:file(helper),
    Pid = spawn(fun() -> init(Servers, MonitorName) end),
    register(Router, Pid),
    Pid.

start(Router, MonitorName) ->
    Pid = defaultStart(Router, MonitorName, []), 
    database:start(),
    startMonitor(Pid, MonitorName).

startWithMonitor(Router, Monitor) ->
    Pid = defaultStart(Router, get_process_alias(Monitor), database:get_servers()),
    io:format("ROUTER::~p@~p:: Spawning router with monitor ~p.~n", [Router, Pid, get_process_alias(Monitor)]),
    Router ! {monitor, Monitor},
    Pid.

init(Servers, MonitorName) ->
    process_flag(trap_exit, true),
    loop(Servers, MonitorName).

startMonitor(Router, MonitorName) ->  
    compile:file(router_monitor),  
    io:format("ROUTER:~p@~p:: Monitor starting: ~p~n", [get_process_alias(Router), Router, MonitorName]),
    MonitorPid = start_monitor(Router, MonitorName),
    Router ! {monitor, MonitorPid},
    MonitorPid.

loop(Servers, MonitorName) ->
    receive
        % Return all servers when requested - WORKING ✅
        {From, servers} ->
            From ! {servers, Servers},
            loop(Servers, MonitorName);
        % Add server to the routing - WORKING ✅
        {add_server, ServerName, Server} ->
            add_server(ServerName, Server),
            loop([{ServerName, Server} | Servers], MonitorName);
        % Connect client to server - WORKING ✅
        {connect, Client, ServerName} ->
            Server = lists:keyfind(ServerName, 1, Servers),
            io:format("Router connecting client ~p to server ~p~n", [Client, ServerName]),
            ServerId = element(2, Server),
            ServerId ! {connect, Client},
            loop(Servers, MonitorName);
        % Monitor messages
        {monitor, Monitor} ->
            request_to_monitor(Monitor),
            loop(Servers, MonitorName);
        % Restart Router Monitor when it goes down
        {'EXIT', Monitor, Reason} ->
            io:format("ROUTER::~p@~p::EXIT:: Router ~p is down due to: ~p~n", [get_process_alias(self()), self(), Monitor, Reason]),
            NewMonitor = startMonitor(self(), MonitorName),  
            loop(Servers, get_process_alias(NewMonitor));
        % Stop the server
        {refreshServer, ServerName, OldServer, NewServer} ->
            io:format("ROUTER::~p@~p:: Refresh server ~p from ~p to ~p~n", [get_process_alias(self()), self(), ServerName, OldServer, NewServer]),
            NewServers = lists:keyreplace(ServerName, 1, Servers, {ServerName, NewServer}),
            io:format("ROUTER::~p@~p:: SERVERS: ~p~n", [get_process_alias(self()), self(), NewServers]),
            loop(NewServers, MonitorName);
        % Stop the router
        stop ->
            io:format("Router stopping~n")
    end.

add_server(ServerName, Server) ->
    io:format("ROUTER::~p@~p:: Trying to add server ~p@~p~n", [get_process_alias(self()), self(), ServerName, Server]),
    Server ! {connected, self()},
    database:add_server({ServerName, Server}),
    io:format("ROUTER::~p@~p:: Server ~p@~p added!~n", [get_process_alias(self()), self(), ServerName, Server]).

request_to_monitor(Monitor) ->
    io:format("ROUTER::~p@~p:: Request to be monitored by ~p~n", [get_process_alias(self()), self(), Monitor]),
    Monitor ! {monitor, self()},
    io:format("ROUTER::~p@~p:: Monitor started on ~p~n", [get_process_alias(self()), self(), get_process_alias(Monitor)]).

% TODO:
% - When a server goes down remove from list
% - When a client goes down remove from server list
% - Validate if a client is connected to a server before sending message
% - Validate if a server is connected to a router before sending message
% - Don't allow a server to connect twice (same as client)