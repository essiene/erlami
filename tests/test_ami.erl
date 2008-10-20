-module(test_ami).
-include_lib("eunit/include/eunit.hrl").

login_fail_test() ->
    amisym:start(),
    ?assertEqual({error, {login, failed}}, ami:new("localhost", 15038, "test", "test")),
    amisym:stop().

login_ok_test() ->
    amisym:start(),
    InterpPid = ami:new("localhost", 15038, "sym", "sym"),
    ?assertEqual(true, is_pid(InterpPid)),
    amisym:stop().

interp_working_test() ->
    amisym:start(),
    InterpPid = ami:new("localhost", 15038, "sym", "sym"), 
    [{response, "Success"} | _Rest] = interp:rpc(InterpPid, [{action, "login"}]),
    amisym:stop().
