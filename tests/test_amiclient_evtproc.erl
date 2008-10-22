-module(test_amiclient_evtproc).
-include_lib("eunit/include/eunit.hrl").

-export([test_handler/2]).




new_test() ->
    EvtProc = amiclient_evtproc:new(),
    ?assertEqual(
        {amiclient_evtproc, evtproc_default, {}}, 
        amiclient_evtproc:handler_get_default(EvtProc)),
    amiclient_evtproc:close(EvtProc).


handler_set_get_test() ->
    EvtProc = amiclient_evtproc:new(),
    amiclient_evtproc:handler_set(EvtProc, handler1, {module1, function1, {arg1, arg2}}),
    ?assertEqual(
        {module1, function1, {arg1, arg2}},
        amiclient_evtproc:handler_get(EvtProc, handler1)),
    amiclient_evtproc:close(EvtProc).

handler_noset_get_test() ->
    EvtProc = amiclient_evtproc:new(),
    ?assertEqual(
        {amiclient_evtproc, evtproc_default, {}},
        amiclient_evtproc:handler_get(EvtProc, handler1)),
    amiclient_evtproc:close(EvtProc).


handler_set_then_handle_test() ->
    EvtProc = amiclient_evtproc:new(),
    ExpectedValue = 55,
    amiclient_evtproc:handler_set(EvtProc, handler1, {?MODULE, test_handler, {self(), ExpectedValue}}),
    spawn(
        fun () -> 
                amiclient_evtproc:handle(EvtProc, [{event, handler1}])
        end
    ),

    receive
        {ok, _Event, Value} ->
            ?assertEqual(ExpectedValue, Value)
    after 2000 ->
            ?assertEqual(ExpectedValue, nil)
    end.



test_handler(Event, {Sender, Value}) ->
    Sender ! {ok, Event, Value}.
