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
    amisym_eventbus:start(),
    Server = amisym_server:start(Port),
    loop(Port, Server).


loop(Port, Server) ->
    receive
        {From, ping} ->
            From ! server_ping(Server),
            loop(Port, Server);
        {From, stop} ->
            {ok, stopped} = amisym_eventbus:stop(),
            ok = amisym_server:stop(Server),
            From ! {?SYM_REG_NAME, ok};
        {'EXIT', _Any, _Reason} ->
            util:logmessage("Restarting all"),
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
