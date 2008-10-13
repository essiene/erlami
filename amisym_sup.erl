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
    register(?SYM_REG_NAME, spawn(?MODULE, init, [Port])).

init(Port) ->
    supervise(Port).

supervise(Port) ->
    ServerPid = amisym_server:start(Port),
    loop(Port, ServerPid).


loop(Port, ServerPid) ->
    receive
        {From, ping} ->
            From ! server_ping(ServerPid),
            loop(Port, ServerPid);
        {From, stop} ->
            From ! {?SYM_REG_NAME, stopping},
            server_stop(ServerPid);
        {'EXIT', _Pid, _Reason} ->
            util:logmessage("Restarting server"),
            supervise(Port);
        _Any ->
            loop(Port, ServerPid)
    end.


server_ping(ServerPid) ->
    case is_process_alive(ServerPid) of
        true ->
            {?SYM_REG_NAME, pong};
        false ->
            {?SYM_REG_NAME, pang}
    end.


server_stop(ServerPid) ->
    exit(ServerPid, kill),
    exit(normal).

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
