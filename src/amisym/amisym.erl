-module(amisym).
-export([
        start/2,
        start/0,
        stop/1
    ]).

-behaviour(application).

start(_Type, StartArgs) ->
    Res = amisym_sup:start(StartArgs),
    error_logger:info_report({application_started, ?MODULE}),
    Res.

stop(_State) ->
    amisym_sup:stop().

start() ->
    start(normal, []).
