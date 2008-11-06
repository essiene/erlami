-module(amievent_manager).
-export([
        start/0,
        stop/1,
        handler_add/3,
        handler_del/3,
        handler_get/1,
        handler_add_default/1,
        event_send/2
    ]).



start() ->
    case gen_event:start() of
        {ok, Pid} ->
            Pid;
        Other ->
            Other
    end.

stop(EvnMngr) ->
    gen_event:stop(EvnMngr).


handler_add(EvnMngr, Module, Args) ->
    case gen_event:add_sup_handler(EvnMngr, Module, Args) of
        ok ->
            ok;
        {'EXIT', Reason} ->
            {error, Reason};
        Other ->
            Other
    end.

handler_add_default(EvnMngr) ->
    handler_add(EvnMngr, amiclient_event_handler, []).

handler_del(EvnMngr, Module, Args) ->
    gen_event:delete_handler(EvnMngr, Module, Args).


handler_get(EvnMngr) ->
    gen_event:which_handlers(EvnMngr).

event_send(EvnMngr, Event) ->
    gen_event:notify(EvnMngr, Event).
