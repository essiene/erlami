-module(amisym_session).
-export([
        new/1,
        init/1
    ]).
-include("ami.hrl").

new(Client) ->
    spawn_link(?MODULE, init, [Client]).

init(Client) ->
    send_banner(Client, ?SYM_BANNER, ?SYM_VERSION),
    Interp = amisym_interp:new(self()),
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
            messaging:send(Client, messaging:amilist_to_block(Response)),
            session(Client, Interp, Remainder);
        {tcp_closed, Client} ->
            gen_tcp:close(Client),
            exit(Interp, {tcp_closed, Client});
        {tcp_error, Client, Reason} ->
            gen_tcp:close(Client),
            exit(Interp, {tcp_error, Client, Reason});
        {tcp, Client, Data} ->
            NewData = string:concat(Remainder, Data),
            {BlockList, NewRemainder} = messaging:get_blocks(NewData, "\r\n\r\n"),
            amisym_interp:interpret_blocks(Interp, BlockList, self()),
            session(Client, Interp, NewRemainder);
        _Any ->
            session(Client, Interp, Remainder)
            
    end.


send_banner(Client, Id, Version) ->
    Banner = string:join([Id, Version], "/"),
    Line = string:concat(Banner, "\r\n"),
    messaging:send(Client, Line).
