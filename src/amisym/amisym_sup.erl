-module(amisym_sup).
-behaviour(supervisor).

-export([
        start/0
    ]).

-export([
        init/1
    ]).

-include("ami.hrl").


start() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

init([]) ->
    ClientSup = {?CLIENT_SUP, 
        {amisym_client_sup, start_link, []},
        permanent, 5000, supervisor, [amisym_client_sup]
    },

    Listener = {?LISTENER,
        {amisym_server, start_link, [15038]},
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
