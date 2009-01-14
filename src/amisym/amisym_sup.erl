-module(amisym_sup).

-behaviour(supervisor).

-export([
            start/0,
            start/1,
            start_link/1,
            stop/0,
            stop/1,
            init/1            
        ]).

-include("ami.hrl").


start_link(Config) when is_tuple(Config) ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, [Config]).

start(Config) when is_tuple(Config) ->
    start_link(Config).
        
start() ->
    start(erlcfg:new()).

stop() ->
    exit(?SUPERVISOR, normal).

stop(Pid) ->
    exit(Pid, normal).

init([Config]) ->
    error_logger:info_report({in_init_supervisor}),
    application:start(crypto),
    application:start(inets),

    ClientSup = {?CLIENT_SUP, 
        {amisym_client_sup, start_link, []},
        permanent, 5000, supervisor, [amisym_client_sup]
    },

    Listener = {?LISTENER,
        {amisym_server, start_link, [Config]},
        permanent, 5000, worker, [amisym_server]
    },

    EventBus = {?EVENT_BUS, 
        {amisym_eventbus, start_link, []},
        permanent, 5000, worker, [amisym_eventbus]
    },


    {
        ok, 
        {
            {one_for_one, 3, 10},
            [EventBus, ClientSup, Listener]
        }
    }.
