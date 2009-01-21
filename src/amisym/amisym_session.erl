-module(amisym_session).
-export([
        new/1,
        close/1,
        send_response/3,
        send_event/2
    ]).

-export([
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3
    ]).

-include("ami.hrl").
-behaviour(gen_server).

new(Client) ->
    gen_server:start_link(?MODULE, Client, []).

close(SessionPid) ->
    gen_server:cast(SessionPid, close).


send_banner(Client, Id, Version) ->
    Banner = string:join([Id, Version], "/"),
    Line = string:concat(Banner, "\r\n"),
    amitcp:send(Client, Line, raw).

send_response(SessionPid, Response, Command) ->
    case amilist:get_value(Command, actionid) of
        {error, {no_key, _Key}} ->
            sendmsg(SessionPid, Response);
        ActionId ->
            Response1 = amilist:set_value(Response, actionid, ActionId),
            sendmsg(SessionPid, Response1)
    end.

send_event(SessionPid, Event) ->
    sendmsg(SessionPid, Event).

sendmsg(SessionPid, Message) ->
    gen_server:cast(SessionPid, {self(), Message}).

% gen_server callbacks

init(Client) ->
    send_banner(Client, ?SYM_BANNER, ?SYM_VERSION),
    Interp = amisym_interp:new(),
    process_flag(trap_exit, true),
    inet:setopts(Client, [{active, once}]),
    {ok, {Client, Interp, ""}}.

handle_call(stop, _From, {_Client, Interp, _Remainder}=State) ->
    interp:close(Interp),
    {stop, stop_requested, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({Interp, Message}, {Client, Interp, Remainder}) ->
    amitcp:send(Client, Message),
    {noreply, {Client, Interp, Remainder}};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'EXIT', Interp, _Reason}, {Client, Interp, Remainder}) ->
            NewInterPid = amisym_interp:new(),
            interp:change_state(NewInterPid),
            {noreply, {Client, NewInterPid, Remainder}};
handle_info({tcp_closed, Client}, {Client, _Interp, _Remainder}=State) ->
    {stop, normal, State};
handle_info({tcp_error, Client, _Reason}, {Client, _Interp, _Remainder}=State) ->
    {stop, normal, State};
handle_info({tcp, Client, Data}, {Client, Interp, Remainder}) ->
    NewData = string:concat(Remainder, Data),
    {BlockList, NewRemainder} = messaging:get_blocks(NewData),
    interp:interpret_blocks(Interp, BlockList),
    inet:setopts(Client, [{active, once}]),
    {noreply, {Client, Interp, NewRemainder}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, {Client, Interp, _Remainder}) ->
    gen_tcp:close(Client),
    interp:close(Interp).

code_change(_OldVsn, {_Client, _Interp, _Remainder}=State, _Extra) ->
    {noreply, State}.
