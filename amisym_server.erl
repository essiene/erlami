-module(amisym_server).
-export([
        start/0,
        start/1,
        new/1,
        serve_forever/1,
        session_new/2
    ]).
-include("ami.hrl").


start() ->
    start(15038).

start(Port) ->
    Server = new(Port),
    spawn(amisym_server, serve_forever, [Server]).


new(Port) ->
    case gen_tcp:listen(Port, ?SYM_SERVER_OPTION) of
        {ok, ListenSocket} ->
            ListenSocket;
        {error, Reason} ->
            throw({new, Reason})
    end.

serve_forever(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {error, Reason} ->
            throw({accept, Reason});
        {ok, Client} ->
            Pid = spawn(?MODULE, session_new, [Client, self()]),
            link(Pid),
            case gen_tcp:controlling_process(Client, Pid) of
                {error, Reason} ->
                    util:logmessage({error, Reason}),
                    exit(Pid, Reason);
                ok ->
                    Pid ! {self(), continue}
            end,
            serve_forever(ListenSocket)
    end.

session_new(Client, PPid) ->
    receive
        {PPid, continue} ->
            send_banner(Client, ?SYM_BANNER, ?SYM_VERSION),
            session(Client)
    after 5000 ->
            exit({controllin_process, timeout})
    end.

session(Client) ->
    inet:setopts(Client, [{active, once}]),
    receive
        {tcp_closed, Client} ->
            gen_tcp:close(Client);
        {tcp_error, Client, _Reason} ->
            gen_tcp:close(Client);
        {tcp, Client, Data} ->
            util:logmessage(Data),
            messaging:tcp_send(Client, Data),
            session(Client)
    end.


send_banner(Client, Id, Version) ->
    Banner = string:join([Id, Version], "/"),
    Line = string:concat(Banner, "\r\n"),
    messaging:tcp_send(Client, Line).
