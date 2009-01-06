-module(amiclient_event_handler).
-behaviour(gen_event).
-export([
        init/1,
        handle_event/2,
        handle_call/2,
        handle_info/2,
        terminate/2,
        code_change/3
    ]).


init([]) ->
    util:logmessage("default handler started"),
    {ok, []}.


handle_event(Event, State) ->
    util:logmessage(Event),
    {ok, State}.

handle_call(Request, State) ->
    {ok, {illegal_request, Request}, State}.

handle_info(Info, State) ->
    util:logmessage({recieved_illegal, Info}),
    {ok, State}.

terminate(_Args, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
