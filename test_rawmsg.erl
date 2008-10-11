-module(test_rawmsg).
-include_lib("eunit/include/eunit.hrl").

append_mark_empty_test() ->
    ?assertEqual("", rawmsg:append_mark("")).

append_mark_test() ->
    ?assertEqual("test_stringPROCESSORMARK", rawmsg:append_mark("test_string")).
