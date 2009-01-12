-module(amisym_server).

-behaviour(gen_listener_tcp).

-include("ami.hrl").

-export([
        start/0,
        start/1,
        stop/1,
        serve/2
    ]).

-export([
            init/1
        ]).


start() ->
    gen_listener_tcp:start({local, ?LISTENER}, ?MODULE, [erlcfg:new()], []). 

start(Port) when is_integer(Port) ->
    gen_listener_tcp:start({local, ?LISTENER}, ?MODULE, [Port, 10], []);

start(Config) when is_tuple(Config) ->
    gen_listener_tcp:start_link({local, ?LISTENER}, ?MODULE, [Config], []).

stop(ServerPid) ->
    gen_listener_tcp:stop({local, ?LISTENER}).    

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
            ],
            
            {
                amisym_client_sup,
                start_child,
                []
            }
         }
    }.
