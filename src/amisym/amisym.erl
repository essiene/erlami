-module(amisym).
-export([
        start/1,
        start/0,
        stop/0,
        ping/0
    ]).

-include("ami.hrl").


start() ->
    amisym_sup:start().

start(Port) ->
    amisym_sup:start(Port).

stop() ->
    amisym_sup:rpc(stop).

ping() ->
    amisym_sup:rpc(ping).
