-module(server).
-export([start/1, connectToRouter/4]).
 
start(Server) ->
    Pid = spawn(fun() -> loop([]) end),
    register(Server, Pid).

% Connect to the respective router
connectToRouter(Server, Name, Router, Remote) -> Server ! {connect_to_router, Name, Router, Remote}.
 
loop(Clients) ->
    receive
        % Connect to router - WORKING ✅
        {connect_to_router, Name, Router, Remote} ->
            net_adm:ping(Remote),
            {Router, Remote} ! {Name, self(), add_server},
            loop(Clients);
        % Receive sucess message from the router connection - WORKING ✅
        {connected, Router} ->
            io:format("Router connected: ~p~n", [Router]),
            loop(Clients);
        % Connect client to server - WORKING ✅
        {connect, Client} ->
            io:format("Client connected: ~p~n", [Client]),
            loop([Client | Clients]);
        % Receive message from client and broadcast to all clients - WORKING ✅
        {From, Message} ->
            io:format("Server received message: ~p From: ~p ~n", [Message, From]),
            lists:foreach(fun(Client) -> Client ! {From, {server, Message}} end, lists:delete(From, Clients)),
            loop(Clients);
        % Stop the server
        stop ->
            io:format("Server stopping~n")
    end.