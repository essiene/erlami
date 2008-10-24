-module(amiclient_evtproc).
-export([
        new/0,
        close/1
    ]).

-export([
        handler_set/3,
        handler_set_default/2,
        handler_del/2,
        handler_get/2,
        handler_get_default/1,
        handle/2
    ]).

-export([
        evtproc_default/2
    ]).


new() ->
    TableName = list_to_atom("evtproc_" ++ pid_to_list(self())),
    EvtProc = ets:new(TableName, [protected]),
    handler_set_default(EvtProc, {?MODULE, evtproc_default, {}}),
    EvtProc.

close(EvtProc) ->
    ets:delete(EvtProc).

handler_set(EvtProc, EventName, {Module, Fun, Args}) ->
    ets:insert(EvtProc, {EventName, {Module, Fun, Args}}).

handler_set_default(EvtProc, {_Module, _Fun, _Args}=Handler) ->
    handler_set(EvtProc, 'evtproc_default', Handler).

handler_get(EvtProc, EventName) ->
    HandlerList = ets:match_object(EvtProc, {EventName, '_'}),
    case HandlerList of
        [] ->
            [{'evtproc_default', {Module, Fun, Args}}] = ets:match_object(EvtProc, {evtproc_default, '_'}),
            {Module, Fun, Args};
        [{EventName, {Module, Fun, Args}}] ->
            {Module, Fun, Args}
    end.

handler_get_default(EvtProc) ->
    handler_get(EvtProc, 'evtproc_default').

handler_del(EvtProc, EventName) ->
    ets:delete(EvtProc, EventName).

handle(EvtProc, [{event, EventName} | _Rest] = Event) ->
    {Module, Fun, Args} = handler_get(EvtProc, EventName),
    apply(Module, Fun, [Event, Args]).

evtproc_default(_Event, _Args) ->
    %util:logmessage("---- No Handler ----"),
    %util:logmessage(Event),
    %util:logmessage("--------------------").
    do_nothing.
