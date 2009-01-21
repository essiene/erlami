-module(amisym).
-export([
        start/0,
        stop/0
    ]).

-export([
        start/2,
        stop/1
    ]).

-behaviour(application).

start() ->
    application:start(erlami).

stop() ->
    application:stop(erlami).

% Application Callbacks

start(_Type, _StartArgs) ->
    amisym_sup:start().

stop(_State) ->
    ok.
