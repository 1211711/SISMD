% Recevied Pid, ServerId and message From Client 
% Send the message to the server

-module(router).

-import(router_monitor,[start_monitor/3]).

-export([start/2, startWithMonitor/2]).

start(Router, Monitor) ->
    Pid = spawn(fun() -> loop([]) end),
    startMonitor(Pid, Router, Monitor),
    process_flag(trap_exit, true),
    register(Router, Pid).

startWithMonitor(Router, Monitor) ->
    Pid = spawn(fun() -> loop([]) end),
    io:format("[ROUTER] Spawning router ~p.~n", [Pid]),
    Monitor ! {monitor, self()},
    register(Router, Pid).

startMonitor(Router, RouterName, Monitor) ->  
    compile:file(router_monitor),  
    MonitorPid = start_monitor(Router, RouterName, Monitor),
    io:format("Monitor started: ~p~n", [Monitor]),
    Router ! {monitor, MonitorPid}.

loop(Servers) ->
    receive
        % Return all servers when requested - WORKING ✅
        {From, servers} ->
            From ! {servers, Servers},
            loop(Servers);
        % Add server to the routing - WORKING ✅
        {Name, Server, add_server} ->
            Server ! {connected, self()},
            io:format("Router adding server: ~p@~p~n", [Name,Server]),
            loop([{Name, Server} | Servers]);
        % Connect client to server - WORKING ✅
        {connect, Client, ServerName} ->
            Server = lists:keyfind(ServerName, 1, Servers),
            io:format("Router connecting client ~p to server ~p~n", [Client, ServerName]),
            ServerId = element(2, Server),
            ServerId ! {connect, Client},
            loop(Servers);
        % Clients sends message to server - WORKING ✅
        {ServerName, Client, Message} ->
            Server = lists:keyfind(ServerName, 1, Servers),
            io:format("Client ~p sending message: ~p to ~p~n", [Client, Message, Server]),
            ServerId = element(2, Server),
            ServerId ! {Client, Message},
            loop(Servers);
        % Monitor messages
        {monitor, Monitor} ->
            io:format("[ROUTER] Request to monitor...~n"),
            Monitor ! {monitor, self()},
            loop(Servers);
        % Stop the router
        stop ->
            io:format("Router stopping~n")
    end.


% TODO:
% - When a server goes down remove from list
% - When a client goes down remove from server list
% - Validate if a client is connected to a server before sending message
% - Validate if a server is connected to a router before sending message
% - Don't allow a server to connect twice (same as client)