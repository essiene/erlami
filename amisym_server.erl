-module(amisym_server).
-export([
        start/0,
        start/1,
        new/1,
        serve/1
    ]).
-include("ami.hrl").


start() ->
    start(15038).

start(Port) ->
    Server = new(Port),
    process_flag(trap_exit, true),
    spawn_link(amisym_server, serve, [Server]).


new(Port) ->
    case gen_tcp:listen(Port, ?SYM_SERVER_OPTION) of
        {ok, ListenSocket} ->
            ListenSocket;
        {error, Reason} ->
            throw({new, Reason})
    end.

serve(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {error, Reason} ->
            throw({accept, Reason});
        {ok, Client} ->
            SessionPid = amisym_session:create(Client, self()),
            case gen_tcp:controlling_process(Client, SessionPid) of
                {error, Reason} ->
                    util:logmessage({error, Reason}),
                    exit(SessionPid, kill);
                ok ->
                    amisym_session:start(SessionPid)
            end,
            serve(ListenSocket)
    end.
