-module(test_ami).
-include_lib("eunit/include/eunit.hrl").

login_fail_test() ->
    amisym:start(),
    ?assertEqual({error, {login, failed}}, ami:new("localhost", 15038, "test", "test")),
    amisym:stop().

login_ok_test() ->
    amisym:start(),
    {SessionPid, InterpPid} = ami:new("localhost", 15038, "sym", "sym"),
    ?assert(SessionPid =/= self()),
    ?assertEqual(true, is_pid(SessionPid)),
    ?assertEqual(true, is_pid(InterpPid)),
    amisym:stop().

execute_working_test() ->
    amisym:start(),
    Ami = ami:new("localhost", 15038, "sym", "sym"), 
    [{response, "Success"} | _Rest] = ami:execute(Ami, [{action, "login"}]),
    amisym:stop().
