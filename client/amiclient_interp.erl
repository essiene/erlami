-module(amiclient_interp).
-export([
        new/0,
        state_not_logged_in/1,
        state_logged_in/4
    ]).


new() ->
    SessionPid = self(),
    spawn_link(?MODULE, state_not_logged_in, [SessionPid]).

state_not_logged_in(SessionPid) ->
    receive
        {SessionPid, {cmd, exit}} ->
            do_nothing;
        {SessionPid, [{response, "Success"} | _Rest]} ->
            SessionPid ! {self(), {login, ok}},
            EvtProc = amiclient_evtproc:new(),
            state_logged_in(SessionPid, EvtProc, 0, ets:new(actionid_pidmap, [private]));
        {SessionPid, [{response, "Error"} | _Rest]} ->
            SessionPid ! {self(), {login, failed}};
        _Any ->
            state_not_logged_in(SessionPid)
    end.

state_logged_in(SessionPid, EvtProc, Tid, SenderMap) ->
    receive 
        {SessionPid, [{response, _Status} | _Rest] = Response} ->
            ActionId = amilist:get_value(Response, actionid),
            [{ActionId, SenderPid}] = ets:lookup(SenderMap, ActionId),
            SenderPid ! {self(), Response},
            ets:delete(SenderMap, ActionId),
            state_logged_in(SessionPid, EvtProc, Tid, SenderMap);
        {SessionPid, [{event, _EventName} | _Rest] = Event} ->
            amiclient_evtproc:handle(EvtProc, Event),
            state_logged_in(SessionPid, EvtProc, Tid, SenderMap);
        {From, [{action, _Action} | _Rest] = Cmd} ->
            ActionId = Tid + 1,
            NewCmd = amilist:set_value(Cmd, actionid, ActionId),
            SessionPid ! {self(), NewCmd},
            ets:insert(SenderMap, {integer_to_list(ActionId), From}),
            state_logged_in(SessionPid, EvtProc, ActionId, SenderMap);
        {From, {evtproc_handler_set, EventName, Handler}} ->
            amiclient_evtproc:handler_set(EvtProc, EventName, Handler),
            From ! {self(), {handler_set, EventName, Handler}},
            state_logged_in(SessionPid, EvtProc, Tid, SenderMap);
        {From, {evtproc_handler_del, EventName}} ->
            amiclient_evtproc:handler_del(EvtProc, EventName),
            From ! {self(), {handler_del, EventName}},
            state_logged_in(SessionPid, EvtProc, Tid, SenderMap);
        {From, close} ->
            SessionPid ! {self(), {close, close_requested}},
            receive
                {SessionPid, {ok, closed}} ->
                    ok
            end,
            From ! {self(), {ok, closed}};
        _Any ->
            state_logged_in(SessionPid, EvtProc, Tid, SenderMap)
    end.
