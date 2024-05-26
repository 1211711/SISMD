-module(server).

-import(server_monitor,[startMonitor/2]).
-import(helper,[get_process_alias/1]).

-export([start/2, connectToRouter/3, startWithMonitor/2]).

defaultStart(Server, MonitorName, Servers) ->
    compile:file(helper),
    compile:file(server_monitor),  
    Pid = spawn(fun() -> init(Servers, MonitorName) end),
    register(Server, Pid),
    Pid.

start(ServerName, MonitorName) ->
    Pid = defaultStart(ServerName, MonitorName, []),
    enableMonitoring(Pid, MonitorName).

startWithMonitor(ServerName, Monitor) ->
    Pid = defaultStart(ServerName, get_process_alias(Monitor), []),
    io:format("SERVER::~p@~p:: Spawning server with monitor ~p.~n", [ServerName, Pid, get_process_alias(Monitor)]),
    Pid ! {monitor, Monitor},
    Pid.

startWithMonitor(ServerName, Monitor, FullRouter) ->
    Pid = defaultStart(ServerName, get_process_alias(Monitor), []),
    io:format("SERVER::~p@~p:: Spawning server with monitor ~p.~n", [ServerName, Pid, get_process_alias(Monitor)]),
    Pid ! {monitor, Monitor, FullRouter},
    Pid.


enableMonitoring(Server, MonitorName) -> 
    MonitorPid = createMonitor(Server, MonitorName),
    Server ! {monitor, MonitorPid},
    MonitorPid.


enableMonitoring(Server, MonitorName, FullRouter) -> 
    MonitorPid = createMonitor(Server, MonitorName),
    Server ! {monitor, MonitorPid, FullRouter},
    MonitorPid.

createMonitor(Server, MonitorName) -> 
    compile:file(server_monitor),
    io:format("SERVER:~p@~p:: Monitor starting: ~p~n", [get_process_alias(Server), Server, MonitorName]),
    startMonitor(Server, MonitorName).

init(Clients, MonitorName) ->
    process_flag(trap_exit, true),
    loop(Clients, MonitorName, "").

% Connect to the respective router
connectToRouter(Server, Router, Remote) -> Server ! {connect_to_router, Router, Remote}.
 
loop(Clients, MonitorName, FullRouter) ->
    receive
        % Connect to router - WORKING ✅
        {connect_to_router, Router, Remote} ->
            net_adm:ping(Remote),
            io:format("SERVER::~p@~p:: Trying to connect to ~p~n", [get_process_alias(self()), self(), Router]),
            {Router, Remote} ! {add_server, get_process_alias(self()), self()},
            whereis(MonitorName) ! {add_router, Router, Remote},
            loop(Clients, MonitorName, {Router, Remote});
        % Receive sucess message from the router connection - WORKING ✅
        {connected, Router} ->
            io:format("SERVER::~p@~p:: Router ~p connected.~n", [get_process_alias(self()), self(), Router]),
            loop(Clients, MonitorName, FullRouter);
        % Connect client to server - WORKING ✅
        {connect, Client} ->
            io:format("SERVER::~p@~p:: Client ~p connected.~n", [get_process_alias(self()), self(), Client]),
            loop([Client | Clients], MonitorName, FullRouter);
        % Receive message from client and broadcast to all clients - WORKING ✅
        {broadcast, From, Message} ->
            io:format("SERVER::~p@~p:: Received message ~p from ~p.~n", [get_process_alias(self()), self(), Message, From]),
            lists:foreach(fun(Client) -> Client ! {From, {server, Message}} end, lists:delete(From, Clients)),
            loop(Clients, MonitorName, FullRouter);
        % Monitor messages
        {monitor, Monitor} ->
            request_to_monitor(Monitor),
            loop(Clients, MonitorName, FullRouter);
        % Monitor messages
        {monitor, Monitor, ThisFullRouter} ->
            request_to_monitor(Monitor, ThisFullRouter),
            loop(Clients, MonitorName, ThisFullRouter);
        % Monitor messages
        revive_monitor ->
            io:format("SERVER::~p@~p:: Revive monitor request. Router: ~p~n", [get_process_alias(self()), self(), FullRouter]),
            enableMonitoring(self(), MonitorName, FullRouter),
            loop(Clients, MonitorName, FullRouter);
        % Stop the server
        stop ->
            io:format("Server stopping~n")
    end.

request_to_monitor(Monitor) ->
    io:format("SERVER::~p@~p:: Request to be monitored by ~p~n", [get_process_alias(self()), self(), Monitor]),
    Monitor ! {monitor, self()},
    io:format("SERVER::~p@~p:: Monitor started on ~p~n", [get_process_alias(self()), self(), get_process_alias(Monitor)]).

request_to_monitor(Monitor, FullRouter) ->
    io:format("SERVER::~p@~p:: Request to be monitored by ~p~n", [get_process_alias(self()), self(), Monitor]),
    Monitor ! {monitor, self(), FullRouter},
    io:format("SERVER::~p@~p:: Monitor started on ~p~n", [get_process_alias(self()), self(), get_process_alias(Monitor)]).

% - Validate if client is connected to the server
% - Feedback to client when connecting