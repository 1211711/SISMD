-module(server).

-import(server_monitor,[startMonitor/2]).
-import(helper,[get_process_alias/1]).

-export([start/2, connectToRouter/4, startWithMonitor/2]).

defaultStart(Server, MonitorName, Servers) ->
    compile:file(helper),
    compile:file(server_monitor),  
    Pid = spawn(fun() -> init(Servers, MonitorName) end),
    register(Server, Pid),
    Pid.

start(ServerName, MonitorName) ->
    Pid = defaultStart(ServerName, MonitorName, []),
    enableMonitoring(Pid, MonitorName).

startWithMonitor(Server, Monitor) ->
    Pid = defaultStart(Server, get_process_alias(Monitor), []),
    io:format("SERVER::~p@~p:: Spawning server with monitor ~p.~n", [Server, Pid, get_process_alias(Monitor)]),
    Server ! {monitor, Monitor},
    Pid.

enableMonitoring(Server, MonitorName) ->  
    compile:file(server_monitor),  
    io:format("SERVER:~p@~p:: Monitor starting: ~p~n", [get_process_alias(Server), Server, MonitorName]),
    MonitorPid = startMonitor(Server, MonitorName),
    Server ! {monitor, MonitorPid},
    MonitorPid.

init(Clients, MonitorName) ->
    process_flag(trap_exit, true),
    loop(Clients, MonitorName).

% Connect to the respective router
connectToRouter(Server, Name, Router, Remote) -> Server ! {connect_to_router, Name, Router, Remote}.
 
loop(Clients, MonitorName) ->
    receive
        % Connect to router - WORKING ✅
        {connect_to_router, Name, Router, Remote} ->
            net_adm:ping(Remote),
            io:format("Server::~p@~p:: Trying to connect to ~p~n", [get_process_alias(self()), self(), Router]),
            {Router, Remote} ! {add_server, Name, self()},
            whereis(MonitorName) ! {Router, Remote},
            loop(Clients, MonitorName);
        % Receive sucess message from the router connection - WORKING ✅
        {connected, Router} ->
            io:format("Router connected: ~p~n", [Router]),
            loop(Clients, MonitorName);
        % Connect client to server - WORKING ✅
        {connect, Client} ->
            io:format("Client connected: ~p~n", [Client]),
            loop([Client | Clients], MonitorName);
        % Receive message from client and broadcast to all clients - WORKING ✅
        {broadcast, From, Message} ->
            io:format("Server received message: ~p From: ~p ~n", [Message, From]),
            lists:foreach(fun(Client) -> Client ! {From, {server, Message}} end, lists:delete(From, Clients)),
            loop(Clients, MonitorName);
        % Monitor messages
        {monitor, Monitor} ->
            request_to_monitor(Monitor),
            loop(Clients, MonitorName);
        % Stop the server
        stop ->
            io:format("Server stopping~n")
    end.

request_to_monitor(Monitor) ->
    io:format("SERVER::~p@~p:: Request to be monitored by ~p~n", [get_process_alias(self()), self(), Monitor]),
    Monitor ! {monitor, self()},
    io:format("SERVER::~p@~p:: Monitor started on ~p~n", [get_process_alias(self()), self(), get_process_alias(Monitor)]).