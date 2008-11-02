-module(amiclient_interp).
-export([
        new/0
    ]).

-export([
        init/1,
        handle_event/3,
        handle_sync_event/4,
        handle_info/3,
        terminate/3,
        code_change/4
    ]).

-export([
        insecure/2,
        secure/2,
        secure/3
    ]).

-behaviour(gen_fsm).


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

insecure({SessionPid, close}, SessionPid) ->
    {stop, normal, SessionPid};
insecure({SessionPid, [{response, "Success"} | _Rest]}, SessionPid) ->
    SessionPid ! {self(), {login, ok}},
    EvtProc = amiclient_evtproc:new(), % should this be a gen_event manager?
    {next_state, secure, {SessionPid, EvtProc, 0, ets:new(actionid_pidmap, [private])}};
insecure({SessionPid, [{response, "Error"} | _Rest]}, SessionPid) ->
    SessionPid ! {self(), {login, failed}},
    {stop, normal, SessionPid}.

% secure state
secure({_From, close}, {SessionPid, _EvtProc, _Tid, _SenderMap}=State) ->
    SessionPid ! {self(), {close, close_requested}},
    {stop, normal, State};
secure({SessionPid, [{response, _Status} | _Rest] = Response}, {SessionPid, _EvtProc, _Tid, SenderMap}=State) ->
    ActionId = amilist:get_value(Response, actionid),
    [{ActionId, Sender}] = ets:lookup(SenderMap, ActionId),
    gen_fsm:reply(Sender, Response),
    ets:delete(SenderMap, ActionId),
    {next_state, secure, State};
secure({SessionPid, [{event, _EventName} | _Rest] = Event}, {SessionPid, EvtProc, _Tid, _SenderMap}=State) ->
    amiclient_evtproc:handle(EvtProc, Event),
    {next_state, secure, State}.

secure({evtproc_handler_set, EventName, Handler}, _From, {_SessionPid, EvtProc, _Tid, _SenderMap}=State) ->  % Consider making an Ami() = {SessionPid, InterpPid, EventHandlerPid}
    amiclient_evtproc:handler_set(EvtProc, EventName, Handler),
    {reply, {handler_set, EventName, Handler}, secure, State};
secure({evtproc_handler_del, EventName}, _From, {_SessionPid, EvtProc, _Tid, _SenderMap}=State) ->
    amiclient_evtproc:handler_del(EvtProc, EventName),
    {reply, {handler_del, EventName}, secure, State};
secure([{action, _Action} | _Rest]= Cmd, From, {SessionPid, EvtProc, Tid, SenderMap}) ->
    ActionId = Tid + 1,
    NewCmd = amilist:set_value(Cmd, actionid, ActionId),
    SessionPid ! {self(), NewCmd},
    ets:insert(SenderMap, {integer_to_list(ActionId), From}),
    {next_state, secure, {SessionPid, EvtProc, ActionId, SenderMap}}.

