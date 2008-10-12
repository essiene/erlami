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

