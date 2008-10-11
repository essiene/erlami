-module(test_rawmsg).
-include_lib("eunit/include/eunit.hrl").

append_mark_empty_test() ->
    ?assertEqual("", rawmsg:append_mark("")).

append_mark_test() ->
    ?assertEqual("test_stringPROCESSORMARK", rawmsg:append_mark("test_string")).


remove_mark_empty_test() ->
    ?assertEqual("", rawmsg:remove_mark("")).

remove_mark_unmarked_test() ->
    ?assertEqual("unmarked_string", rawmsg:remove_mark("unmarked_string")).

remove_mark_marked_test() ->
    ?assertEqual("marked_string", rawmsg:remove_mark("marked_stringPROCESSORMARK")).
