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





get_blocks_and_incomplete_empty_list_test() ->
    ?assertEqual({[], ""}, rawmsg:get_blocks_and_incomplete([])).

get_blocks_and_single_test() ->
    ?assertEqual({["string1"], ""}, rawmsg:get_blocks_and_incomplete(["string1"])).

get_blocks_and_no_incomplete_test() ->
    ?assertEqual({["string1", "string2"], ""}, rawmsg:get_blocks_and_incomplete(["string1", "string2"])).

get_blocks_and_incomplete_test() ->
    ?assertEqual({["string1"], "string2"}, rawmsg:get_blocks_and_incomplete(["string1", "string2PROCESSORMARK"])).

get_blocks_and_multiple_no_incomplete_test() ->
    ?assertEqual({["string1", "string2", "string3", "string4"], ""}, rawmsg:get_blocks_and_incomplete(["string1", "string2", "string3", "string4"])).

get_blocks_multiple_and_incomplete_test() ->
    ?assertEqual({["string1", "string2", "string3"], "string4"}, rawmsg:get_blocks_and_incomplete(["string1", "string2", "string3", "string4PROCESSORMARK"])).






get_blocks_incomplete_test() ->
    ?assertEqual({["string1,string2,string3"], "string4,string5,string6"}, rawmsg:get_blocks("string1,string2,string3SEPstring4,string5,string6", "SEP")).

get_blocks_no_incomplete_test() ->
    ?assertEqual({["string1,string2,string3", "string4,string5,string6"], ""}, rawmsg:get_blocks("string1,string2,string3SEPstring4,string5,string6SEP", "SEP")).
