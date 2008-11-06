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

originate_no_number_test() ->
    amisym:start(),
    Ami = ami:new("localhost", 15038, "sym", "sym"), 
    [{response, "Success"} | _Rest] = ami:originate(Ami, "SIP/pass", local, 111, 1),
    amisym:stop().

originate_prejoined_number_test() ->
    amisym:start(),
    Ami = ami:new("localhost", 15038, "sym", "sym"), 
    [{response, "Success"} | _Rest] = ami:originate(Ami, "SIP/pass/111222333", local, 111, 1),
    amisym:stop().

originate_no_prejoined_number_test() ->
    amisym:start(),
    Ami = ami:new("localhost", 15038, "sym", "sym"), 
    [{response, "Success"} | _Rest] = ami:originate(Ami, "SIP/pass", "111222333", local, 111, 1),
    amisym:stop().

close_test() ->
    amisym:start(),
    {SessionPid, InterpPid} = Ami = ami:new("localhost", 15038, "sym", "sym"),
    ?assertEqual(true, is_process_alive(SessionPid)),
    ?assertEqual(true, is_process_alive(InterpPid)),
    ami:close(Ami),
    timer:sleep(2000),
    ?assertEqual(false, is_process_alive(SessionPid)),
    ?assertEqual(false, is_process_alive(InterpPid)),
    amisym:stop().


