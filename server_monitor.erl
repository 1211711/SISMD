-module(server_monitor).

-import(server, [startWithMonitor/2]).
-import(helper,[get_process_alias/1]).

-export([startMonitor/2]).

startMonitor(Server, MonitorName) -> 
    Pid = spawn(fun() -> init(Server) end),
    io:format("SERVER MONITOR::~p@~p:: Monitor requested for ~p@~p~n", [MonitorName, Pid, get_process_alias(Server), Server]),
    register(MonitorName, Pid),
    Pid.

init(Server) ->
    compile:file(helper),
    process_flag(trap_exit, true),
    loop(Server).

loop(Server) ->
    receive
        % Start Router monitoring
        {monitor, Server} ->
            io:format("SERVER MONITOR::~p@~p:: Server ~p@~p being monitored.~n", [get_process_alias(self()), self(), get_process_alias(Server), Server]),
            link(Server),
            loop(Server, get_process_alias(Server), "")
    end.

loop(Server, ServerName, FullRouter) ->
    receive
        % Restart Router when it goes down
        {'EXIT', Server, Reason} ->
            io:format("SERVER MONITOR::~p@~p::EXIT:: Server ~p@~p is down due to: ~p~n", [get_process_alias(self()), self(), ServerName, Server, Reason]),
            NewServer = start_server(ServerName),
            io:format("FULL ROUTING ~p~n", [FullRouter]),
            FullRouter ! {refreshServer, get_process_alias(NewServer), Server, NewServer},
            loop(NewServer);
        % Add Router
        {Router, Remote} ->
            io:format("SERVER MONITOR::~p@~p:: Router added ~p@~p", [get_process_alias(self()), self(), Router, Remote]),
            loop(Server, ServerName, {Router, Remote})
    end.

start_server(ServerName) -> startWithMonitor(ServerName, self()).