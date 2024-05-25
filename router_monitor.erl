-module(router_monitor).

-import(router, [startWithMonitor/2]).
-import(helper,[get_process_alias/1]).

-export([start_monitor/2]).

start_monitor(Router, MonitorName) -> 
    compile:file(helper),  
    Pid = spawn(fun() -> init(Router) end),
    io:format("ROUTER MONITOR::~p@~p:: Monitor requested for ~p@~p~n", [MonitorName, Pid, get_process_alias(Router), Router]),
    register(MonitorName, Pid),
    Pid.

init(Router) ->
    process_flag(trap_exit, true),
    loop(Router).

loop(Router) ->
    receive
        % Start Router monitoring
        {monitor, Router} ->
            io:format("ROUTER MONITOR::~p@~p:: Router ~p@~p being monitored.~n", [get_process_alias(self()), self(), get_process_alias(Router), Router]),
            link(Router),
            loop(Router, get_process_alias(Router))
    end.

loop(Router, RouterName) ->
    receive
        % Restart Router when it goes down
        {'EXIT', Router, Reason} ->
            io:format("ROUTER MONITOR::~p@~p::EXIT:: Router ~p@~p is down due to: ~p~n", [get_process_alias(self()), self(), RouterName, Router, Reason]),
            NewRouter = start_router(RouterName),
            loop(NewRouter)
    end.

start_router(RouterName) -> startWithMonitor(RouterName, self()).
