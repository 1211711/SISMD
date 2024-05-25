-module(router_monitor).

-import(router, [startWithMonitor/2]).

-export([start_monitor/3]).

start_monitor(Router, RouterName, Monitor) -> 
    Pid = spawn(fun() -> init(Router, RouterName) end),
    register(Monitor, Pid),
    Pid.

init(Router, RouterName) ->
    process_flag(trap_exit, true),
    loop(Router, RouterName).

loop(Router, RouterName) ->
    receive
        % Start Router monitoring
        {monitor, Router} ->
            io:format("[ROUTER MONITOR1] Router ~p being monitored.~n", [Router]),
            link(Router),
            io:format("[ROUTER MONITOR2] Router ~p being monitored.~n", [Router]),
            loop(Router, RouterName);
        % Restart Router when it goes down
        {'EXIT', Router, Reason} ->
            io:format("[EXIT1] Router ~p is down due to: ~p~n", [Router, Reason]),
            NewRouter = start_router(RouterName),  
            loop(NewRouter, RouterName)
    end.

start_router(RouterName) -> startWithMonitor(RouterName, self()).
