-module(test_amisym).
-include_lib("eunit/include/eunit.hrl").


setup_test() ->
    amisym:start().

amisym_test() ->
    ami:new("localhost", 15038, "sym", "sym").

amisym_already_logged_in_test() ->
    Ami = ami:new("localhost", 15038, "sym", "sym"),
    Response = ami:execute(Ami, [{action, login}]),
    ?assertEqual("Success", amilist:get_value(Response, response)),
    ?assertEqual("Already logged in", amilist:get_value(Response, message)).

amisym_already_logout_test() ->
    Ami = ami:new("localhost", 15038, "sym", "sym"),
    Response = ami:execute(Ami, [{action, logout}]),
    ?assertEqual("Success", amilist:get_value(Response, response)),
    ?assertEqual("Logged out", amilist:get_value(Response, message)).

teardown_test() ->
    amisym:stop().
