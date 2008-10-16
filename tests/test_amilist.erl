-module(test_amilist).
-include_lib("eunit/include/eunit.hrl").


new_test() ->
    ?assertEqual([], amilist:new()).

set_value_on_emptylist_test() ->
    AmiList = amilist:new(),
    ?assertEqual([{key1, "Value1"}], amilist:set_value(AmiList, key1, "Value1")).

set_value_on_list_of_one_test() ->
    AmiList = amilist:new(),
    AmiList1 = amilist:set_value(AmiList, key1, "Value1"),
    ?assertEqual([{key1, "Value1"}, {key2, "Value2"}], amilist:set_value(AmiList1, key2, "Value2")).

set_value_on_list_of_multiple_test() ->
    AmiList = amilist:new(),
    AmiList1 = amilist:set_value(AmiList, key1, "Value1"),
    AmiList2 = amilist:set_value(AmiList1, key2, "Value2"),
    AmiList3 = amilist:set_value(AmiList2, key3, "Value3"),
    ?assertEqual([{key1, "Value1"}, {key2, "Value2"}, {key3, "Value3"}, {key4, "Value4"}], amilist:set_value(AmiList3, key4, "Value4")).


has_key_emptylist_test() ->
    AmiList = amilist:new(),
    ?assertEqual(false, amilist:has_key(AmiList, key1)).
