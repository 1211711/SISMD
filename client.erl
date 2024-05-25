-module(client).
 
-export([start/1, add_remote/1, get_servers/3, connect_to_server/4, send_message/5]).
 
start(Client) ->
    Pid = spawn(fun() -> loop() end),
    register(Client, Pid).
 
add_remote(Remote) -> net_adm:ping(Remote).

get_servers(Client, Router, Remote) -> Client ! {servers, Router, Remote}.

connect_to_server(Client, Router, Remote, ServerName) -> Client ! {connect, Router, Remote, ServerName}.

send_message(Client, ServerName, Router, Remote, Message) -> Client ! {send, ServerName, Router, Remote, Message}.
 
loop() ->
    receive
        % Send messages to the router (not done in router yet) - need to add server
        {send, ServerName, Router, Remote, Message} ->
            io:format("Client sending message: ~p to ~p/~p~n", [Message, Router, Remote]),
            {Router, Remote} ! {ServerName, self(), Message},
            loop();
        % Receive messages from the server
        {Server, {server, Message}} ->
            io:format("From: ~p | Client received message: ~p~n", [Server, Message]),
            loop();
        % Ask router to connect to server - WORKING ✅
        {connect, Router, Remote, ServerName} ->
            net_adm:ping(Remote),
            {Router, Remote} ! {connect, self(), ServerName},
            loop();
        % Receive servers from router - WORKING ✅
        {servers, Servers} ->
            io:format("Available servers: ~p~n", [Servers]),
            loop();
        % Ask router for servers - WORKING ✅
        {servers, Router, Remote} ->
            add_remote(Remote),
            {Router, Remote} ! {self(), servers},
            loop();
        % Stop the client
        stop ->
            io:format("Client stopping~n")
    end.
