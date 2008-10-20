-module(test_amisym).
-include_lib("eunit/include/eunit.hrl").



amisym_test() ->
    amisym:start(),
    ami:new("localhost", 15038, "sym", "sym"),
    amisym:stop().
