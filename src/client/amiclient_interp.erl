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

-include("ami.hrl").


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
    {ok, insecure, #client_interp{session=SessionPid}}.

handle_event(_Request, StateName, St) ->
    {next_state, StateName, St}.

handle_sync_event(close, _From, _StateName, St) ->
    {stop, normal, {ok, stopped}, St};

handle_sync_event(Request, _From, StateName, St) ->
    {reply, {illegal_request, Request}, StateName, St}.

handle_info(_Info, StateName, St) ->
    {next_state, StateName, St}.

terminate(_Reason, _StateName, _St) ->
    ok.

code_change(_OldVsn, StateName, St, _Extra) ->
    {next_state, StateName, St}.


% insecure state

insecure(Event, _From, St) ->
    {reply, {illegal_event, Event}, insecure, St}.

insecure({SessionPid, close}, #client_interp{session=SessionPid}=St) ->
    {stop, normal, St};
insecure({SessionPid, [{response, "Success"} | _Rest]}, #client_interp{session=SessionPid}=St) ->
    amiclient_session:login_ok(SessionPid),

    StNew = St#client_interp{
        evt_mngr=amievent_manager:start(),
        senders=ets:new(actionid_pidmap, [private])
    },

    {next_state, secure, StNew};
insecure({SessionPid, [{response, "Error"} | _Rest]}, #client_interp{session=SessionPid}=St) ->
    amiclient_session:login_failed(SessionPid),
    {stop, normal, St};
insecure(_Event, St) ->
    {next_state, insecure, St}.

% secure state
secure({SessionPid, [{response, _Status} | _Rest] = Response}, #client_interp{session=SessionPid}=St) ->
    ActionId = amilist:get_value(Response, actionid),
    [{ActionId, Sender}] = ets:lookup(St#client_interp.senders, ActionId),
    gen_fsm:reply(Sender, Response),
    ets:delete(St#client_interp.senders, ActionId),
    {next_state, secure, St};
secure({SessionPid, [{event, _EventName} | _Rest] = Event}, #client_interp{session=SessionPid}=St) ->
    amievent_manager:event_send(St#client_interp.evt_mngr, Event),
    {next_state, secure, St};
secure(_Event, St) ->
    {next_state, secure, St}.

secure({handler_add, Handler, Args}, _From, St) ->  
    amievent_manager:handler_add(St#client_interp.evt_mngr, Handler, Args),
    {reply, {handler_added, Handler}, secure, St};
secure({handler_del, Handler}, _From, St) ->
    amievent_manager:handler_del(St#client_interp.evt_mngr, Handler),
    {reply, {handler_deleted, Handler}, secure, St};
secure(handler_get, _From, St) ->
    Handlers = amievent_manager:handler_get(St#client_interp.evt_mngr),
    {reply, Handlers, secure, St};
secure([{action, _Action} | _Rest]= Cmd, From, St) ->
    ActionId = St#client_interp.tid + 1,
    NewCmd = amilist:set_value(Cmd, actionid, ActionId),
    amiclient_session:send_command(St#client_interp.session, NewCmd),
    ets:insert(St#client_interp.senders, {integer_to_list(ActionId), From}),
    {next_state, secure, St#client_interp{tid=ActionId}};
secure(Event, _From, St) ->
    {reply, {illegal_event, Event}, secure, St}.
