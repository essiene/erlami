-module(amisym_client_sup).
-behaviour(supervisor).

-include("ami.hrl").

-export([
            start_link/0,
            start_child/1
        ]).

-export([
            init/1
        ]).


start_link() ->
    Res = supervisor:start_link({local, ?CLIENT_SUP}, ?MODULE, []),
    error_logger:info_report({?MODULE, started}),
    Res.

start_child(ClientSocket) ->
    supervisor:start_child(?CLIENT_SUP, [ClientSocket]).

init([]) ->
    {ok,
        {
            {simple_one_for_one, 3, 10},
            [
                {
                    amisym_client_sup,
                    {amisym_session, new, []},
                    transient,
                    5000,
                    worker,
                    [amisym_session]
                }
            ]
        }
    }.
