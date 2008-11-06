-module(amiclient_interp).
-export([
        init/1,
        handle_event/3,
        handle_sync_event/4,
        handle_info/3,
        terminate/3,
        code_change/4
    ]).

-export([
        new/0,
        insecure/2,
        insecure/3,
        secure/2,
        secure/3
    ]).

-behaviour(gen_fsm).
-behaviour(ami_interp).


new() ->
    SessionPid = self(),
    case gen_fsm:start_link(?MODULE, SessionPid, []) of
        {ok, Pid} ->
            Pid;
        Any ->
            Any
    end.

% gen_fsm callbacks

init(SessionPid) ->
    {ok, insecure, SessionPid}.

handle_event(_Request, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(Request, _From, StateName, State) ->
    {reply, {illegal_request, Request}, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {next_state, StateName, State}.


% insecure state

insecure(Event, _From, State) ->
    {reply, {illegal_event, Event}, insecure, State}.

insecure({SessionPid, close}, SessionPid) ->
    {stop, normal, SessionPid};
insecure({SessionPid, [{response, "Success"} | _Rest]}, SessionPid) ->
    SessionPid ! {self(), {login, ok}},
    EventMgr = amievent_manager:start(),
    {next_state, secure, {SessionPid, EventMgr, 0, ets:new(actionid_pidmap, [private])}};
insecure({SessionPid, [{response, "Error"} | _Rest]}, SessionPid) ->
    SessionPid ! {self(), {login, failed}},
    {stop, normal, SessionPid};
insecure(_Event, State) ->
    {next_state, insecure, State}.

% secure state
secure({_From, close}, {SessionPid, _EventMgr, _Tid, _SenderMap}=State) ->
    SessionPid ! {self(), {close, close_requested}},
    {stop, normal, State};
secure({SessionPid, [{response, _Status} | _Rest] = Response}, {SessionPid, _EventMgr, _Tid, SenderMap}=State) ->
    ActionId = amilist:get_value(Response, actionid),
    [{ActionId, Sender}] = ets:lookup(SenderMap, ActionId),
    gen_fsm:reply(Sender, Response),
    ets:delete(SenderMap, ActionId),
    {next_state, secure, State};
secure({SessionPid, [{event, _EventName} | _Rest] = Event}, {SessionPid, EventMgr, _Tid, _SenderMap}=State) ->
    amievent_manager:event_send(EventMgr, Event),
    {next_state, secure, State};
secure(_Event, State) ->
    {next_state, secure, State}.

secure({handler_add, Handler, Args}, _From, {_SessionPid, EventMgr, _Tid, _SenderMap}=State) ->  % Consider making an Ami() = {SessionPid, InterpPid, EventHandlerPid}
    amievent_manager:handler_add(EventMgr, Handler, Args),
    {reply, {handler_added, Handler}, secure, State};
secure({handler_del, Handler}, _From, {_SessionPid, EventMgr, _Tid, _SenderMap}=State) ->
    amievent_manager:handler_del(EventMgr, Handler),
    {reply, {handler_deleted, Handler}, secure, State};
secure(handler_get, _From, {_SessionPid, EventMgr, _Tid, _SenderMap}=State) ->
    Handlers = amievent_manager:handler_get(EventMgr),
    {reply, Handlers, secure, State};
secure([{action, _Action} | _Rest]= Cmd, From, {SessionPid, EventMgr, Tid, SenderMap}) ->
    ActionId = Tid + 1,
    NewCmd = amilist:set_value(Cmd, actionid, ActionId),
    SessionPid ! {self(), NewCmd},
    ets:insert(SenderMap, {integer_to_list(ActionId), From}),
    {next_state, secure, {SessionPid, EventMgr, ActionId, SenderMap}};
secure(Event, _From, State) ->
    {reply, {illegal_event, Event}, secure, State}.
