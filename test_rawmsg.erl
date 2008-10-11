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





split_test() ->
    ?assertEqual(["string_one", "string_two", "string_three"], rawmsg:split("string_oneSEPstring_twoSEPstring_three", "SEP")).

split_no_seperator_test() ->
    ?assertEqual(["string_one"], rawmsg:split("string_one", "SEP")).

split_empty_string_test() ->
    ?assertEqual([], rawmsg:split("", "SEP")).

split_terminating_seperator_test() ->
    ?assertEqual(["string_one", "string_two"], rawmsg:split("string_oneSEPstring_twoSEP", "SEP")).

split_single_part_terminating_seperator_test() ->
    ?assertEqual(["string_one"], rawmsg:split("string_oneSEP", "SEP")).

split_seperator_only_test() ->
    ?assertEqual([], rawmsg:split("SEP", "SEP")).

