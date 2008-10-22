-module(amisym_session).
-export([
        new/1,
        init/1,
        send_response/3,
        send_event/2
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
        {Interp, Message} ->
            amitcp:send(Client, Message),
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

send_response(SessionPid, Response, Command) ->
    case amilist:get_value(Command, actionid) of
        {error, {no_key, _Key}} ->
            SessionPid ! {self(), Response};
        ActionId ->
            Response1 = amilist:set_value(Response, actionid, ActionId),
            SessionPid ! {self(), Response1}
    end.

send_event(SessionPid, Event) ->
    SessionPid ! {self(), Event}.
