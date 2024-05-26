-module(client).

-import(helper,[get_process_alias/1]).
 
-export([start/1, add_remote/1, get_servers/3, add_server/4, send_message/2, connect_server/2]).
 
start(Client) ->
    Pid = spawn(fun() -> loop() end),
    register(Client, Pid).
 
add_remote(Remote) -> net_adm:ping(Remote).

add_server(Client, Router, Remote, ServerName) -> Client ! {add, Router, Remote, ServerName}.

get_servers(Client, Router, Remote) -> Client ! {servers, Router, Remote}.

connect_server(Client, Server) -> Client ! {connect_server, Server}.

send_message(Client, Message) -> Client ! {send, Message}.
 
loop() ->
    receive
        % Connect temporarily to server
        {connect_server, Server} ->
            io:format("CLIENT::~p@~p:: Connected to server: ~p~n", [get_process_alias(self()), self(), Server]),
            loop(Server);
        % Ask router to connect to server - WORKING ✅
        {add, Router, Remote, ServerName} ->
            net_adm:ping(Remote),
            {Router, Remote} ! {connect, self(), ServerName},
            loop();
        % Ask router for servers - WORKING ✅
        {servers, Router, Remote} ->
            add_remote(Remote),
            {Router, Remote} ! {self(), servers},
            loop();
        % Receive servers from router - WORKING ✅
        {servers, Servers} ->
            io:format("Available servers: ~p~n", [Servers]),
            loop();
        % Stop the client
        stop ->
            io:format("Client stopping~n")
    end.

loop(CurrentServer) ->
    receive
        % Send messages to the router
        {send, Message} ->
            io:format("CLIENT::~p@~p:: Sending message: ~p to ~p~n", [get_process_alias(self()), self(), Message, CurrentServer]),
            CurrentServer ! {broadcast, self(), Message},
            loop(CurrentServer);
        % Receive messages from the server
        {Server, {server, Message}} ->
            io:format("CLIENT::~p@~p:: Received message: ~p from ~p~n", [get_process_alias(self()), self(), Message, Server]),
            loop(CurrentServer);
        disconnect ->
            io:format("CLIENT::~p@~p:: Disconnecting from server: ~p~n", [get_process_alias(self()), self(), CurrentServer]),
            loop()
    end.