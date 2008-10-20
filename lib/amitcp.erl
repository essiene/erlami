-module(amitcp).
-export([
        create_server/1,
        wait_for_connection/1,
        set_controlling_process/2,
        send/2,
        send/3,
        connect/2
    ]).

-include("ami.hrl").

create_server(Port) ->
    case gen_tcp:listen(Port, ?SYM_SERVER_OPTION) of
        {ok, ListenSocket} ->
            ListenSocket;
        {error, Reason} ->
            throw({create_server, Reason})
    end.

wait_for_connection(Server) ->
    case gen_tcp:accept(Server) of
        {ok, Client} ->
            Client;
        {error, Reason} ->
            throw({wait_for_connection, Reason})
    end.

set_controlling_process(Socket, Pid) ->
    case gen_tcp:controlling_process(Socket, Pid) of
        ok ->
            ok;
        {error, Reason} ->
            throw({set_controlling_process, Reason})
    end.

send(Socket, AmiList) ->
    Block = messaging:amilist_to_block(AmiList),
    send(Socket, Block, raw).

send(Socket, Data, raw) ->
    case gen_tcp:send(Socket, Data) of
        {error, Reason} ->
            throw({send, Reason});
        ok ->
            ok
    end.

connect(Host, Port) ->
    case gen_tcp:connect(Host, Port, ?CONNECT_OPTION) of
        {ok, Socket} -> 
            Socket;
        {error, Reason} ->
            throw({connect, Reason})
    end.


