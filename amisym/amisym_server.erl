-module(amisym_server).
-export([
        start/0,
        start/1,
        new/1,
        listen/1
    ]).
-include("ami.hrl").


start() ->
    start(15038).

start(Port) ->
    Server = new(Port),
    spawn_link(?MODULE, listen, [Server]).


new(Port) ->
    case gen_tcp:listen(Port, ?SYM_SERVER_OPTION) of
        {ok, ListenSocket} ->
            ListenSocket;
        {error, Reason} ->
            throw({new, Reason})
    end.

listen(ListenSocket) ->
    process_flag(trap_exit, true),
    serve(ListenSocket).


serve(ListenSocket) ->
    {ok, Client} = gen_tcp:accept(ListenSocket),
    SessionPid = amisym_session:new(Client),
    gen_tcp:controlling_process(Client, SessionPid),
    serve(ListenSocket).
