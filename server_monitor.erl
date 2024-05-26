-module(server_monitor).

-import(server, [startWithMonitor/2,startWithMonitor/3]).
-import(helper,[get_process_alias/1]).

-export([startMonitor/2,startMonitor/3]).

startMonitor(Server, MonitorName) -> 
    Pid = spawn(fun() -> init(Server) end),
    io:format("SERVER MONITOR::~p@~p:: Monitor requested for ~p@~p~n", [MonitorName, Pid, get_process_alias(Server), Server]),
    register(MonitorName, Pid),
    Pid.

startMonitor(Server, MonitorName, FullRouter) -> 
    Pid = spawn(fun() -> init(Server, FullRouter) end),
    io:format("SERVER MONITOR::~p@~p:: Monitor requested for ~p@~p~n", [MonitorName, Pid, get_process_alias(Server), Server]),
    register(MonitorName, Pid),
    Pid.

init(Server) ->
    compile:file(helper),
    process_flag(trap_exit, true),
    loop(Server, get_process_alias(Server)).

init(Server, FullRouter) ->
    compile:file(helper),
    process_flag(trap_exit, true),
    loop(Server, get_process_alias(Server), FullRouter).

loop(Server, ServerName, FullRouter) ->
    receive
        % Start Router monitoring
        {monitor, Server} ->
            io:format("SERVER MONITOR::~p@~p:: Server ~p@~p being monitored.~n", [get_process_alias(self()), self(), get_process_alias(Server), Server]),
            link(Server),
            FullRouter ! {add_server_monitor, get_process_alias(Server), self()},
            loop(Server, get_process_alias(Server), FullRouter);
        % Restart Router when it goes down
        {'EXIT', DownServer, Reason} ->
            io:format("SERVER MONITOR::~p@~p::EXIT:: Server ~p@~p is down due to: ~p~n", [get_process_alias(self()), self(), ServerName, DownServer, Reason]),
            NewServer = startWithMonitor(ServerName, self(), FullRouter),
            FullRouter ! {refreshServer, get_process_alias(NewServer), DownServer, NewServer},
            loop(NewServer, ServerName, FullRouter);
        % Add Router
        {add_router, Router, Remote} ->
            io:format("SERVER MONITOR::~p@~p:: Adding router ~p@~p.~n", [get_process_alias(self()), self(), Router, Remote]),
            {Router, Remote} ! {add_server_monitor, get_process_alias(Server), self()},
            io:format("SERVER MONITOR::~p@~p:: Router added ~p@~p.~n", [get_process_alias(self()), self(), Router, Remote]),
            loop(Server, get_process_alias(Server), {Router, Remote})
    end.

loop(Server, ServerName) ->
    receive
        % Start Router monitoring
        {monitor, Server} ->
            io:format("SERVER MONITOR::~p@~p:: Server ~p@~p being monitored.~n", [get_process_alias(self()), self(), get_process_alias(Server), Server]),
            link(Server),
            loop(Server, get_process_alias(Server));
        % Start Full Router monitoring
        {monitor, Server, ThisFullRouter} ->
            io:format("SERVER MONITOR::~p@~p:: Server ~p@~p being monitored.~n", [get_process_alias(self()), self(), get_process_alias(Server), Server]),
            link(Server),
            ThisFullRouter ! {add_server_monitor, get_process_alias(Server), self()},
            loop(Server, get_process_alias(Server), ThisFullRouter);
        % Restart Router when it goes down
        {'EXIT', DownServer, Reason} ->
            io:format("SERVER MONITOR::~p@~p::EXIT:: Server ~p@~p is down due to: ~p~n", [get_process_alias(self()), self(), ServerName, DownServer, Reason]),
            NewServer = startWithMonitor(ServerName, self()),
            loop(NewServer, ServerName);
        % Add Router
        {add_router, Router, Remote} ->
            io:format("SERVER MONITOR::~p@~p:: Adding router ~p@~p.~n", [get_process_alias(self()), self(), Router, Remote]),
            {Router, Remote} ! {add_server_monitor, get_process_alias(Server), self()},
            io:format("SERVER MONITOR::~p@~p:: Router added ~p@~p.~n", [get_process_alias(self()), self(), Router, Remote]),
            loop(Server, get_process_alias(Server), {Router, Remote})
    end.

% - Server going down without being connected to router gives error due to FullRouter ! {refreshServer, get_process_alias(NewServer), DownServer, NewServer}, (FULLROUTER IS NULL)
% - Support router connected response (just like server).
% - Case: connect to router, restart router, connect again -> fail
% - Add full router when restarting from router 
% - Need to do the refreshServer differently