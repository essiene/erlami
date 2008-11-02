-module(amisym_sup).
-export([
        start/0,
        start/1,
        init/1,
        rpc/1
    ]).
-include("ami.hrl").

start() ->
    start(15038).

start(Port) ->
    case whereis(?SYM_REG_NAME) of
        undefined -> 
            register(?SYM_REG_NAME, spawn(?MODULE, init, [Port])),
            {ok, started};
        _ ->
            {ok, already_running}
    end.


init(Port) ->
    process_flag(trap_exit, true),
    supervise(Port).

supervise(Port) ->
    Server = amisym_server:start(Port),
    loop(Port, Server).


loop(Port, Server) ->
    receive
        {From, ping} ->
            From ! server_ping(Server),
            loop(Port, Server);
        {From, stop} ->
            From ! {?SYM_REG_NAME, stopping},
            server_stop(Server);
        {'EXIT', Server, _Reason} ->
            util:logmessage("Restarting server"),
            supervise(Port);
        _Any ->
            loop(Port, Server)
    end.


server_ping(ServerPid) ->
    case is_process_alive(ServerPid) of
        true ->
            {?SYM_REG_NAME, pong};
        false ->
            {?SYM_REG_NAME, pang}
    end.


server_stop(ServerPid) ->
    amisym_server:stop(ServerPid).

rpc(Cmd) ->
    case whereis(?SYM_REG_NAME) of
        undefined ->
            {error, not_alive};
        _Pid ->
            ?SYM_REG_NAME ! {self(), Cmd},
            receive
                {?SYM_REG_NAME, Response} ->
                    Response
            end
    end.
