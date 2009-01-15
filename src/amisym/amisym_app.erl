-module(amisym_app).

-export([
            start/0,
            stop/0
        ]).

start() ->
    application:start(amisym).

stop() ->
    application:stop(amisym).
