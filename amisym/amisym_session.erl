-module(amisym_session).
-export([
        create/2,
        init/2,
        start/1,
        send_blocks/3
    ]).
-include("ami.hrl").

create(Client, PPid) ->
    spawn_link(?MODULE, init, [Client, PPid]).

init(Client, PPid) ->
    receive
        {PPid, continue} ->
            send_banner(Client, ?SYM_BANNER, ?SYM_VERSION),
            InterpPid = amisym_interp:start(self()),
            process_flag(trap_exit, true),
            session(Client, InterpPid, "")
    after 5000 ->
            exit({controlling_process, timeout})
    end.

start(SessionPid) ->
    SessionPid ! {self(), continue}.

session(Client, InterpPid, Remainder) ->
    inet:setopts(Client, [{active, once}]),
    receive
        {'EXIT', InterpPid, _Reason} ->
            NewInterPid = amisym_interp:start(self()),
            NewInterPid ! {self(), {cmd, change_state}},
            session(Client, NewInterPid, Remainder);
        {InterpPid, Response} ->
            messaging:send(Client, messaging:amilist_to_block(Response)),
            session(Client, InterpPid, Remainder);
        {tcp_closed, Client} ->
            gen_tcp:close(Client),
            exit(InterpPid, {tcp_closed, Client});
        {tcp_error, Client, Reason} ->
            gen_tcp:close(Client),
            exit(InterpPid, {tcp_error, Client, Reason});
        {tcp, Client, Data} ->
            NewData = string:concat(Remainder, Data),
            {BlockList, NewRemainder} = messaging:get_blocks(NewData, "\r\n\r\n"),
            async_send_blocks(BlockList, self(), InterpPid),
            session(Client, InterpPid, NewRemainder);
        _Any ->
            session(Client, InterpPid, Remainder)
            
    end.


send_banner(Client, Id, Version) ->
    Banner = string:join([Id, Version], "/"),
    Line = string:concat(Banner, "\r\n"),
    messaging:send(Client, Line).


async_send_blocks(ListOfBlocks, SessionPid, InterpPid) ->
    spawn(?MODULE, send_blocks, [ListOfBlocks, SessionPid, InterpPid]).

send_blocks([], _SessionPid, _InterpPid) -> 
    true;
send_blocks([Block | Rest], SessionPid, InterpPid) ->
    AmiList = messaging:block_to_amilist(Block),
    case amilist:has_key(AmiList, action) of
        true ->
            amisym_interp:send(InterpPid, SessionPid, AmiList);
        false ->
            util:logmessage("****Invalid Message****"),
            util:logmessage(AmiList),
            util:logmessage("***********************")
    end,
    send_blocks(Rest, SessionPid, InterpPid).
