-module(test_ami_socket).
-include("ami.hrl").
-include_lib("eunit/include/eunit.hrl").

setup_test() ->
    amisym:start().


connect_test() ->
    {ok, Sock} = ami_socket:connect("localhost", 15038, [inet, binary, {active, false}]),
    ?assertEqual(true, is_record(Sock, ami_socket)),
    ?assertEqual(true, is_process_alive(Sock#ami_socket.pid)).

getopts_ami_test() ->
    {ok, Sock} = ami_socket:connect("localhost", 15038, [inet, binary, {active, false}, {ami_retry, 10000}]),
    ?assertEqual({ok, [{ami_host, "localhost"}, 
            {ami_port, 15038},
            {ami_retry, 10000}]}, ami_socket:getopts(Sock, [ami_host, ami_port, ami_retry])).

getopts_gentcp_set_disconnected_test() ->
    {ok, Sock} = ami_socket:connect("localhost", 15039, [inet, binary, {active, false}, {ami_retry, 10000}, {delay_send, false}]),
    ?assertEqual({ok, [{active, false}, 
            {delay_send, false}]}, ami_socket:getopts(Sock, [active, delay_send])).

getopts_gentcp_unset_disconnected_test() ->
    {ok, Sock} = ami_socket:connect("localhost", 15039, [inet, binary, {ami_retry, 10000}]),
    ?assertEqual({error, einval}, ami_socket:getopts(Sock, [active, delay_send])).

getopts_mixed_set_disconnected_test() ->
    {ok, Sock} = ami_socket:connect("localhost", 15039, [inet, binary, {active, false}, {ami_retry, 10000}, {delay_send, false}]),
    ?assertEqual({ok, [{active, false}, {ami_retry, 10000},
            {delay_send, false}, {ami_port, 15039}]}, ami_socket:getopts(Sock, [active, ami_retry, delay_send, ami_port])).

getopts_mixed_unset_disconnected_test() ->
    {ok, Sock} = ami_socket:connect("localhost", 15039, [inet, binary, {ami_retry, 10000}]),
    ?assertEqual({error, einval}, ami_socket:getopts(Sock, [active, ami_retry, delay_send, ami_port])).

getopts_mixed_unset_connected_test() ->
    {ok, Sock} = ami_socket:connect("localhost", 15038, [inet, binary, {active, false}, {ami_retry, 10000}]),
    ?assertEqual({ok, [{active, false}, {ami_retry, 10000},
            {delay_send, false}, {ami_port, 15038}]}, ami_socket:getopts(Sock, [active, ami_retry, delay_send, ami_port])).

teardown_test() ->
    amisym:stop().
