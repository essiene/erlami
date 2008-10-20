-module(amisym_server).
-export([
        start/0,
        start/1,
        init/1,
        serve/1
    ]).


start() ->
    start(15038).

start(Port) ->
    Server = amitcp:create_server(Port),
    spawn_link(?MODULE, init, [Server]).

init(Server) ->
    process_flag(trap_exit, true),
    serve(Server).

serve(Server) ->
    try
        Client = amitcp:wait_for_connection(Server),
        amisym_session:new(Client),
        serve(Server)
    catch
        Type: Exception ->
            {Type, Exception}
    after
        gen_tcp:close(Server)
    end.
