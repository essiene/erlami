-module(amisym_server).
-behaviour(gen_listener_tcp).
-include("ami.hrl").

-export([
        start/0,
        start/1,
        start_link/1
    ]).

-export([
            init/1,
            handle_accept/2,
            handle_call/3,
            handle_cast/2,
            handle_info/2,
            terminate/2,
            code_change/3
        ]).

start() ->
    start(15038).

start(Port) ->
    gen_listener_tcp:start({local, ?LISTENER}, ?MODULE, [Port, 10], []).

start_link(Port) ->
    gen_listener_tcp:start_link({local, ?LISTENER}, ?MODULE, [Port, 10], []).

init([Port, Backlog]) ->
    {
        ok, 
        {
            Port,
            [
                list,
                inet, 
                {active, false},
                {backlog, Backlog},
                {reuseaddr, true}
            ]
         },
         nil
    }.

handle_accept(Sock, State) ->
    amisym_client_sup:start_child(Sock),
    {noreply, State}.

handle_call(Request, _From, State) ->
    {reply, {illegal_request, Request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
