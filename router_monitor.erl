-module(router_monitor).

-import(router, [startWithMonitor/2]).

-export([start_monitor/3]).

start_monitor(Router, RouterName, Monitor) -> 
    Pid = spawn(fun() -> loop(Router, RouterName) end),
    process_flag(trap_exit, true),
    register(Monitor, Pid),
    Pid.

loop(Router, RouterName) ->
    receive
        % Start Router monitoring
        {monitor, Router} ->
            io:format("[ROUTER MONITOR] Router ~p being monitored.~n", [Router]),
            link(Router),
            loop(Router, RouterName);
        % Restart Router when it goes down
        {'EXIT', Router, Reason} ->
            io:format("[EXIT] Router ~p is down due to: ~p~n", [Router, Reason]),
            startWithMonitor(RouterName, self()),  
            io:format("[EXIT] Router ~p is down due to: ~p~n", [Router, Reason]),
            loop(Router, RouterName);
        % Stop the router
        stop ->
            io:format("Router stopping~n")
    end.