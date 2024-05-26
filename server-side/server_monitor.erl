-module(server_monitor).

-import(server, [startWithMonitor/3,startWithMonitor/4]).
-import(helper,[get_process_alias/1]).

-export([startMonitor/3,startMonitor/4]).

startMonitor(Server, MonitorName, Clients) -> 
    Pid = spawn(fun() -> init(Server, Clients) end),
    io:format("SERVER MONITOR::~p@~p:: Monitor requested for ~p@~p~n", [MonitorName, Pid, get_process_alias(Server), Server]),
    register(MonitorName, Pid),
    io:format("AQUI~n"),
    Pid.

startMonitor(Server, MonitorName, FullRouter, Clients) -> 
    Pid = spawn(fun() -> init(Server, FullRouter, Clients) end),
    io:format("SERVER MONITOR::~p@~p:: Monitor requested for ~p@~p~n", [MonitorName, Pid, get_process_alias(Server), Server]),
    register(MonitorName, Pid),
    io:format("AQUI~n"),
    Pid.

init(Server, Clients) ->
    compile:file('../helper'),
    process_flag(trap_exit, true),
    loop(Server, get_process_alias(Server), Clients).

init(Server, FullRouter, Clients) ->
    compile:file('../helper'),
    process_flag(trap_exit, true),
    loop(Server, get_process_alias(Server), FullRouter, Clients).

loop(Server, ServerName, FullRouter, Clients) ->
    receive
        % Start Router monitoring
        {monitor, Server} ->
            io:format("SERVER MONITOR::~p@~p:: Server ~p@~p being monitored.~n", [get_process_alias(self()), self(), get_process_alias(Server), Server]),
            link(Server),
            loop(Server, get_process_alias(Server), FullRouter, Clients);
        % Restart Router when it goes down
        {'EXIT', DownServer, Reason} ->
            io:format("SERVER MONITOR::~p@~p::EXIT:: Server ~p@~p is down due to: ~p~n", [get_process_alias(self()), self(), ServerName, DownServer, Reason]),
            NewServer = startWithMonitor(ServerName, self(), FullRouter, Clients),
            FullRouter ! {refreshServer, get_process_alias(NewServer), DownServer, NewServer},
            loop(NewServer, ServerName, FullRouter, Clients);
        % Add Router
        {add_router, Router, Remote} ->
            io:format("SERVER MONITOR::~p@~p:: Adding router ~p@~p.~n", [get_process_alias(self()), self(), Router, Remote]),
            {Router, Remote} ! {add_server_monitor, get_process_alias(Server), self()},
            io:format("SERVER MONITOR::~p@~p:: Router added ~p@~p.~n", [get_process_alias(self()), self(), Router, Remote]),
            loop(Server, get_process_alias(Server), {Router, Remote}, Clients);
        % Connect client to server - WORKING ✅
        {add_client, Client} ->
            io:format("SERVER MONITOR::~p@~p:: Adding client ~p.~n", [get_process_alias(self()), self(), Client]),
            NewClients= [Client | lists:delete(Client, Clients)],
            io:format("SERVER::~p@~p:: Clients connected:~p~n", [get_process_alias(self()), self(), NewClients]),
            loop(Server, get_process_alias(Server), FullRouter, NewClients)
    end.

loop(Server, ServerName, Clients) ->
    receive
        % Start Router monitoring
        {monitor, Server} ->
            io:format("SERVER MONITOR::~p@~p:: Server ~p@~p being monitored.~n", [get_process_alias(self()), self(), get_process_alias(Server), Server]),
            link(Server),
            loop(Server, get_process_alias(Server), Clients);
        % Start Full Router monitoring
        {monitor, Server, ThisFullRouter} ->
            io:format("SERVER MONITOR::~p@~p:: Server ~p@~p being monitored.~n", [get_process_alias(self()), self(), get_process_alias(Server), Server]),
            link(Server),
            ThisFullRouter ! {add_server_monitor, get_process_alias(Server), self()},
            loop(Server, get_process_alias(Server), ThisFullRouter, Clients);
        % Restart Router when it goes down
        {'EXIT', DownServer, Reason} ->
            io:format("SERVER MONITOR::~p@~p::EXIT:: Server ~p@~p is down due to: ~p~n", [get_process_alias(self()), self(), ServerName, DownServer, Reason]),
            NewServer = startWithMonitor(ServerName, self(), Clients),
            loop(NewServer, ServerName, Clients);
        % Add Router
        {add_router, Router, Remote} ->
            io:format("SERVER MONITOR::~p@~p:: Adding router ~p@~p.~n", [get_process_alias(self()), self(), Router, Remote]),
            {Router, Remote} ! {add_server_monitor, get_process_alias(Server), self()},
            io:format("SERVER MONITOR::~p@~p:: Router added ~p@~p.~n", [get_process_alias(self()), self(), Router, Remote]),
            loop(Server, get_process_alias(Server), {Router, Remote}, Clients)
    end.

% - Support router connected response (just like server).
% - Case: connect to router, restart router, connect again -> fail


% Server down -> Monitor down -> Error⚠️