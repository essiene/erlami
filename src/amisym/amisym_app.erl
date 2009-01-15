-module(amisym_app).

-export([
            start/0
        ]).

start() ->
    application:start(amisym).
