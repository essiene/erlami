-module(test_amievent_manager).
-behaviour(gen_event).
-include_lib("eunit/include/eunit.hrl").

-export([
        init/1,
        handle_event/2,
        handle_call/2,
        handle_info/2,
        terminate/2,
        code_change/3
    ]).




new_test() ->
    EvtMgr = amievent_manager:start(),
    amievent_manager:stop(EvtMgr).


handler_set_get_test() ->
    EvtMgr = amievent_manager:start(),
    amievent_manager:handler_add(EvtMgr, amiclient_event_handler, []),
    ?assertEqual([amiclient_event_handler], amievent_manager:handler_get(EvtMgr)),
    amievent_manager:stop(EvtMgr).

handler_noset_get_test() ->
    EvtMgr = amievent_manager:start(),
    ?assertEqual([], amievent_manager:handler_get(EvtMgr)),
    amievent_manager:stop(EvtMgr).

handler_default_set_get_test() ->
    EvtMgr = amievent_manager:start(),
    amievent_manager:handler_add_default(EvtMgr),
    ?assertEqual([amiclient_event_handler], amievent_manager:handler_get(EvtMgr)),
    amievent_manager:stop(EvtMgr).


handler_set_then_handle_test() ->
    EvtMgr = amievent_manager:start(),

    Value = 55,
    Detail = 10,
    Event = [{event, expected}, {details, Detail}],
    Event2 = [{event, unexpected}, {details, Detail}],

    amievent_manager:handler_add(EvtMgr, ?MODULE, [self(), Value]),

    amievent_manager:event_send(EvtMgr, Event),
    ?assertEqual({Value, Detail}, get_response()),

    amievent_manager:event_send(EvtMgr, Event2),
    ?assertEqual({unexpected, Event2}, get_response()),

    amievent_manager:stop(EvtMgr).

get_response() ->
    receive
        {ok, Response} ->
            Response;
        {error, Error} ->
            Error;
        _Any ->
            get_response()
    after 2000 ->
            timed_out
    end.



init([Sender, Value]) ->
    {ok, {Sender, Value}}.


handle_event([{event, expected}, {details, Detail}], {Sender, Value}) ->
    Sender ! {ok, {Value, Detail}},
    {ok, {Sender, Value}};
handle_event(Event, {Sender, Value}) ->
    Sender ! {error, {unexpected, Event}},
    {ok, {Sender, Value}}.

handle_call(Request, State) ->
    {ok, {illegal_request, Request}, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Args, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
