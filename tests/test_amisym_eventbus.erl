-module(test_amisym_eventbus).
-include_lib("eunit/include/eunit.hrl").


amisym_eventbus_new_test() ->
    ?assertEqual({ok, started}, amisym_eventbus:start()),
    Result = whereis(amisym_eventbus),
    ?assert(Result =/= undefined).

amisym_eventbus_connect_test() ->
    amisym_eventbus:start(),
    amisym_eventbus:connect(),
    ?assertEqual({ok, true}, amisym_eventbus:is_connected()).

amisym_eventbus_double_start_test() ->
    amisym_eventbus:start(),
    ?assertEqual({ok, already_running}, amisym_eventbus:start()).


amisym_eventbus_disconnected_test() ->
    amisym_eventbus:start(),
    amisym_eventbus:connect(),
    amisym_eventbus:disconnect(),
    ?assertEqual({ok, false}, amisym_eventbus:is_connected()),
    amisym_eventbus:stop().
