-module(test_util).
-include_lib("eunit/include/eunit.hrl").



split_test() ->
    ?assertEqual(["string_one", "string_two", "string_three"], util:split("string_oneSEPstring_twoSEPstring_three", "SEP")).

split_no_seperator_test() ->
    ?assertEqual(["string_one"], util:split("string_one", "SEP")).

split_empty_string_test() ->
    ?assertEqual([], util:split("", "SEP")).

split_terminating_seperator_test() ->
    ?assertEqual(["string_one", "string_two"], util:split("string_oneSEPstring_twoSEP", "SEP")).

split_single_part_terminating_seperator_test() ->
    ?assertEqual(["string_one"], util:split("string_oneSEP", "SEP")).

split_seperator_only_test() ->
    ?assertEqual([], util:split("SEP", "SEP")).



append_mark_empty_test() ->
    ?assertEqual("", util:append_mark("")).

append_mark_test() ->
    ?assertEqual("test_stringPROCESSORMARK", util:append_mark("test_string")).





remove_mark_empty_test() ->
    ?assertEqual("", util:remove_mark("")).

remove_mark_unmarked_test() ->
    ?assertEqual("unmarked_string", util:remove_mark("unmarked_string")).

remove_mark_marked_test() ->
    ?assertEqual("marked_string", util:remove_mark("marked_stringPROCESSORMARK")).





get_seperated_blocks_and_incomplete_empty_list_test() ->
    ?assertEqual({[], ""}, util:get_seperated_blocks_and_incomplete([])).

get_seperated_blocks_and_single_test() ->
    ?assertEqual({["string1"], ""}, util:get_seperated_blocks_and_incomplete(["string1"])).

get_seperated_blocks_and_no_incomplete_test() ->
    ?assertEqual({["string1", "string2"], ""}, util:get_seperated_blocks_and_incomplete(["string1", "string2"])).

get_seperated_blocks_and_incomplete_test() ->
    ?assertEqual({["string1"], "string2"}, util:get_seperated_blocks_and_incomplete(["string1", "string2PROCESSORMARK"])).

get_seperated_blocks_and_multiple_no_incomplete_test() ->
    ?assertEqual({["string1", "string2", "string3", "string4"], ""}, util:get_seperated_blocks_and_incomplete(["string1", "string2", "string3", "string4"])).

get_seperated_blocks_multiple_and_incomplete_test() ->
    ?assertEqual({["string1", "string2", "string3"], "string4"}, util:get_seperated_blocks_and_incomplete(["string1", "string2", "string3", "string4PROCESSORMARK"])).
