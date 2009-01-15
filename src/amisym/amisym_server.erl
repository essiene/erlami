-module(amisym_server).

-behaviour(gen_listener_tcp).

-include("ami.hrl").

-export([
        start/0,
        start/1,
        start_link/1,
        stop/1,
        serve/2
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

-record(server_state, {
            server_pid,
            session_list
            }).

start_link(Config) ->
    Res = gen_listener_tcp:start_link({local, ?LISTENER}, ?MODULE, [Config], []),
    error_logger:info_report({{?MODULE, started}, {response, Res}}),
    Res.

start() ->
    gen_listener_tcp:start(?LISTENER, ?MODULE, [erlcfg:new()], []). 

start(Port) when is_integer(Port) ->
    gen_listener_tcp:start(?LISTENER, ?MODULE, [Port, 10], []);

start(Config) when is_tuple(Config) ->
    gen_listener_tcp:start_link({local, ?LISTENER}, ?MODULE, [Config], []).

stop(_ServerPid) ->
    gen_listener_tcp:stop({local, ?LISTENER}).    

serve(_, _) ->
    ok.

init([Config]) -> 
    init(Config:get(server.port, 15038), Config:get(server.backlog, 10)).

init(Port, Backlog) ->
    {ok, {
            Port,
            [
                binary,
                inet, 
                {active, false},
                {backlog, Backlog},
                {reuseaddr, true}
            ]
         },
        #server_state{server_pid=self(), session_list=[]}
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
