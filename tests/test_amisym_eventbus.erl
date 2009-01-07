-module(test_amisym_eventbus).
-include_lib("eunit/include/eunit.hrl").


amisym_eventbus_init_test() ->
    {ok, Pid} = amisym_eventbus:start_link(),
    ?assertEqual(is_pid(Pid), true).

amisym_eventbus_connect_test() ->
    amisym_eventbus:start_link(),
    ?assertEqual({ok, connected}, amisym_eventbus:connect()).

amisym_eventbus_disconnect_test() ->
    amisym_eventbus:start_link(),
    amisym_eventbus:connect(),
    ?assertEqual({ok, deleted}, amisym_eventbus:disconnect()).

amisym_eventbus_is_connected_test() ->
    amisym_eventbus:start_link(),
    amisym_eventbus:connect(),
    ?assertEqual({ok, true}, amisym_eventbus:is_connected()).
