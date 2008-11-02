-module(amisym_server).
-export([
        start/0,
        start/1,
        stop/1,
        init/1,
        serve/2
    ]).


start() ->
    start(15038).

start(Port) ->
    Server = amitcp:create_server(Port),
    spawn_link(?MODULE, init, [Server]).

stop(ServerPid) ->
    ServerPid ! close.

init(Server) ->
    process_flag(trap_exit, true),
    amisym_eventbus:start(),
    serve(Server, []).

serve(Server, SessionList) ->
    receive
        close ->
            lists:foreach(
                fun(S) -> amisym_session:close(S) end,
                SessionList
            ),
            exit(normal);
        _Any ->
            do_nothing
    after 0 ->
        try
            Client = amitcp:wait_for_connection(Server),
            Session = amisym_session:new(Client),
            NewSessionList = [Session | SessionList],
            serve(Server, NewSessionList)
        catch
            Type: Exception ->
                {Type, Exception}
        after
            gen_tcp:close(Server),
            amisym_eventbus:stop()
        end
   end.
