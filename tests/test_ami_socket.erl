-module(test_ami_socket).
-include("ami.hrl").
-include_lib("eunit/include/eunit.hrl").


connect_test() ->
    {ok, Sock} = ami_socket:connect("localhost", 1, [inet, binary, {active, false}, {ami_username, "user"}, {ami_secret, "secret"}]),
    ?assertEqual(true, is_record(Sock, ami_socket)),
    ?assertEqual(true, is_process_alive(Sock#ami_socket.pid)).
