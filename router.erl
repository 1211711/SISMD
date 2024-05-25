% Recevied Pid, ServerId and message From Client 
% Send the message to the server

-module(router).
-export([start/1]).

start(Router) ->
    Pid = spawn(fun() -> loop([]) end),
    register(Router, Pid).

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
        % Clients sends message to server
        {ServerName, Client, Message} ->
            Server = lists:keyfind(ServerName, 1, Servers),
            io:format("Client ~p sending message: ~p to ~p~n", [Client, Message, Server]),
            ServerId = element(2, Server),
            ServerId ! {Client, Message},
            loop(Servers);
        stop ->
            io:format("Router stopping~n")
    end.


% TODO:
% - When a server goes down remove from list