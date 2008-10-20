-module(amisym_session).
-export([
        new/1,
        init/1
    ]).
-include("ami.hrl").

new(Client) ->
    SessionPid = spawn_link(?MODULE, init, [Client]),
    amitcp:set_controlling_process(Client, SessionPid).

init(Client) ->
    send_banner(Client, ?SYM_BANNER, ?SYM_VERSION),
    Interp = amisym_interp:new(),
    process_flag(trap_exit, true),
    session(Client, Interp, "").

session(Client, Interp, Remainder) ->
    inet:setopts(Client, [{active, once}]),
    receive
        {'EXIT', Interp, _Reason} ->
            NewInterPid = amisym_interp:new(self()),
            NewInterPid ! {self(), {cmd, change_state}},
            session(Client, NewInterPid, Remainder);
        {Interp, Response} ->
            amitcp:send(Client, Response),
            session(Client, Interp, Remainder);
        {tcp_closed, Client} ->
            gen_tcp:close(Client),
            exit(Interp, {tcp_closed, Client});
        {tcp_error, Client, Reason} ->
            gen_tcp:close(Client),
            exit(Interp, {tcp_error, Client, Reason});
        {tcp, Client, Data} ->
            NewData = string:concat(Remainder, Data),
            {BlockList, NewRemainder} = messaging:get_blocks(NewData),
            interp:interpret_blocks(Interp, BlockList),
            session(Client, Interp, NewRemainder);
        _Any ->
            session(Client, Interp, Remainder)
    end.


send_banner(Client, Id, Version) ->
    Banner = string:join([Id, Version], "/"),
    Line = string:concat(Banner, "\r\n"),
    amitcp:send(Client, Line, raw).
