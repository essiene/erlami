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



lstrip_empty_test() ->
    ?assertEqual("", util:lstrip("")).

lstrip_no_whitespace_test() ->
    ?assertEqual("value", util:lstrip("value")).

lstrip_trailing_whitespace_test() ->
    ?assertEqual("value \r\n", util:lstrip("value \r\n")).

lstrip_test() ->
    ?assertEqual("value", util:lstrip(" \r\n\t\n\rvalue")).

lstrip_leading_and_trailing_ws_test() ->
    ?assertEqual("value \r\n", util:lstrip(" \r\n\t\n\rvalue \r\n")).


rstrip_empty_test() ->
    ?assertEqual("", util:rstrip("")).

rstrip_no_whitespace_test() ->
    ?assertEqual("value", util:rstrip("value")).

rstrip_trailing_whitespace_test() ->
    ?assertEqual("value", util:rstrip("value \r\n")).

rstrip_leading_whitespace_test() ->
    ?assertEqual(" \r\n\t\n\rvalue", util:rstrip(" \r\n\t\n\rvalue")).

rstrip_leading_and_trailing_ws_test() ->
    ?assertEqual(" \r\n\t\n\rvalue", util:rstrip(" \r\n\t\n\rvalue \r\n")).


strip_empty_test() ->
    ?assertEqual("", util:strip("")).

strip_no_whitespace_test() ->
    ?assertEqual("value", util:strip("value")).

strip_trailing_whitespace_test() ->
    ?assertEqual("value", util:strip("value \r\n")).

strip_leading_whitespace_test() ->
    ?assertEqual("value", util:strip(" \r\n\t\n\rvalue")).

strip_leading_and_trailing_ws_test() ->
    ?assertEqual("value", util:strip(" \r\n\t\n\rvalue \r\n")).


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


build_chan_var_empty_test() ->
    ?assertEqual([], util:build_chan_vars([])).

build_chan_var_single_test() ->
    ?assertEqual([{variable, "key1=val1"}], util:build_chan_vars([{key1, "val1"}])).

build_chan_var_multiple_test() ->
    ?assertEqual([
            {variable, "key1=val1"}, 
            {variable, "key2=val2"}, 
            {variable, "key3=val3"}], 
        util:build_chan_vars([
                {key1, "val1"},
                {key2, "val2"},
                {key3, "val3"}
            ])).
