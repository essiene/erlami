-module(test_ami).
-include_lib("eunit/include/eunit.hrl").

setup_test() ->    
    application:start(amisym).

login_fail_test() ->
    ?assertEqual({error, {login, failed}}, ami:new("localhost", 15038, "test", "test")).

login_ok_test() ->
    {SessionPid, InterpPid} = ami:new("localhost", 15038, "sym", "sym"),
    ?assert(SessionPid =/= self()),
    ?assertEqual(true, is_pid(SessionPid)),
    ?assertEqual(true, is_pid(InterpPid)).

execute_working_test() ->
    Ami = ami:new("localhost", 15038, "sym", "sym"), 
    [{response, "Success"} | _Rest] = ami:execute(Ami, [{action, "login"}]).

originate_no_number_test() ->
    Ami = ami:new("localhost", 15038, "sym", "sym"), 
    [{response, "Success"} | _Rest] = ami:originate(Ami, "SIP/pass", local, 111, 1).

originate_prejoined_number_test() ->
    Ami = ami:new("localhost", 15038, "sym", "sym"), 
    [{response, "Success"} | _Rest] = ami:originate(Ami, "SIP/pass/111222333", local, 111, 1).

originate_no_prejoined_number_test() ->
    Ami = ami:new("localhost", 15038, "sym", "sym"), 
    [{response, "Success"} | _Rest] = ami:originate(Ami, "SIP/pass", "111222333", local, 111, 1).

close_test() ->
    {SessionPid, InterpPid} = Ami = ami:new("localhost", 15038, "sym", "sym"),
    ?assertEqual(true, is_process_alive(SessionPid)),
    ?assertEqual(true, is_process_alive(InterpPid)),
    ami:close(Ami),
    timer:sleep(2000),
    ?assertEqual(false, is_process_alive(SessionPid)),
    ?assertEqual(false, is_process_alive(InterpPid)).

teardown_test() ->
    amisym:stop().


