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

get_value_empty_list_test() ->
    AmiList = amilist:new(),
    ?assertEqual({error, {no_key, key1}}, amilist:get_value(AmiList, key1)).

get_value_nonempty_list_nokey_test() ->
    AmiList = amilist:new(),
    AmiList1 = amilist:set_value(AmiList, key1, "Value1"),
    ?assertEqual({error, {no_key, key2}}, amilist:get_value(AmiList1, key2)).

get_value_nonempty_list_key_available_test() ->
    AmiList = amilist:new(),
    AmiList1 = amilist:set_value(AmiList, key1, "Value1"),
    ?assertEqual("Value1", amilist:get_value(AmiList1, key1)).

get_value_multple_entries_list_key_available_test() ->
    AmiList = amilist:new(),
    AmiList1 = amilist:set_value(AmiList, key1, "Value1"),
    AmiList2 = amilist:set_value(AmiList1, key2, "Value2"),
    AmiList3 = amilist:set_value(AmiList2, key3, "Value3"),
    ?assertEqual("Value2", amilist:get_value(AmiList3, key2)).

get_value_multple_entries_list_nokey_test() ->
    AmiList = amilist:new(),
    AmiList1 = amilist:set_value(AmiList, key1, "Value1"),
    AmiList2 = amilist:set_value(AmiList1, key2, "Value2"),
    AmiList3 = amilist:set_value(AmiList2, key3, "Value3"),
    ?assertEqual({error, {no_key, key4}}, amilist:get_value(AmiList3, key4)).

del_value_test() ->
    AmiList = amilist:new(),
    AmiList1 = amilist:set_value(AmiList, key1, "Value1"),
    AmiList2 = amilist:set_value(AmiList1, key2, "Value2"),
    ?assertEqual([{key1, "Value1"}, {key2, "Value2"}], AmiList2),
    AmiList3 = amilist:del_value(AmiList2, key2),
    ?assertEqual([{key1, "Value1"}], AmiList3).


has_key_emptylist_test() ->
    AmiList = amilist:new(),
    ?assertEqual(false, amilist:has_key(AmiList, key1)).

has_key_nonempty_list_nokey_test() ->
    AmiList = amilist:new(),
    AmiList1 = amilist:set_value(AmiList, key1, "Value1"),
    ?assertEqual(false, amilist:has_key(AmiList1, key2)).

has_key_nonempty_list_key_available_test() ->
    AmiList = amilist:new(),
    AmiList1 = amilist:set_value(AmiList, key1, "Value1"),
    ?assertEqual(true, amilist:has_key(AmiList1, key1)).

has_key_multple_entries_list_key_available_test() ->
    AmiList = amilist:new(),
    AmiList1 = amilist:set_value(AmiList, key1, "Value1"),
    AmiList2 = amilist:set_value(AmiList1, key2, "Value2"),
    AmiList3 = amilist:set_value(AmiList2, key3, "Value3"),
    ?assertEqual(true, amilist:has_key(AmiList3, key2)).

has_key_multple_entries_list_nokey_test() ->
    AmiList = amilist:new(),
    AmiList1 = amilist:set_value(AmiList, key1, "Value1"),
    AmiList2 = amilist:set_value(AmiList1, key2, "Value2"),
    AmiList3 = amilist:set_value(AmiList2, key3, "Value3"),
    ?assertEqual(false, amilist:has_key(AmiList3, key4)).

has_value_emptylist_test() ->
    AmiList = amilist:new(),
    ?assertEqual(false, amilist:has_value(AmiList, "Value1")).

has_value_nonempty_list_nokey_test() ->
    AmiList = amilist:new(),
    AmiList1 = amilist:set_value(AmiList, key1, "Value1"),
    ?assertEqual(false, amilist:has_value(AmiList1, "Value2")).

has_value_nonempty_list_key_available_test() ->
    AmiList = amilist:new(),
    AmiList1 = amilist:set_value(AmiList, key1, "Value1"),
    ?assertEqual(true, amilist:has_value(AmiList1, "Value1")).

has_value_multple_entries_list_key_available_test() ->
    AmiList = amilist:new(),
    AmiList1 = amilist:set_value(AmiList, key1, "Value1"),
    AmiList2 = amilist:set_value(AmiList1, key2, "Value2"),
    AmiList3 = amilist:set_value(AmiList2, key3, "Value3"),
    ?assertEqual(true, amilist:has_value(AmiList3, "Value2")).

has_value_multple_entries_list_nokey_test() ->
    AmiList = amilist:new(),
    AmiList1 = amilist:set_value(AmiList, key1, "Value1"),
    AmiList2 = amilist:set_value(AmiList1, key2, "Value2"),
    AmiList3 = amilist:set_value(AmiList2, key3, "Value3"),
    ?assertEqual(false, amilist:has_value(AmiList3, "Value4")).


set_payload_on_emptylist_test() ->
    AmiList = amilist:new(),
    ?assertEqual([{"Payload"}], amilist:set_payload(AmiList, "Payload")).

set_payload_on_list_of_one_test() ->
    AmiList = amilist:new(),
    AmiList1 = amilist:set_value(AmiList, key1, "Value1"),
    ?assertEqual([{key1, "Value1"}, {"Payload"}], amilist:set_payload(AmiList1, "Payload")).

set_payload_on_list_of_multiple_test() ->
    AmiList = amilist:new(),
    AmiList1 = amilist:set_value(AmiList, key1, "Value1"),
    AmiList2 = amilist:set_value(AmiList1, key2, "Value2"),
    AmiList3 = amilist:set_value(AmiList2, key3, "Value3"),
    ?assertEqual([{key1, "Value1"}, {key2, "Value2"}, {key3, "Value3"}, {"Payload"}], amilist:set_payload(AmiList3, "Payload")).


get_payload_empty_list_test() ->
    AmiList = amilist:new(),
    ?assertEqual({error, no_payload}, amilist:get_payload(AmiList)).

get_payload_nonempty_list_nopayload_test() ->
    AmiList = amilist:new(),
    AmiList1 = amilist:set_value(AmiList, key1, "Value1"),
    ?assertEqual({error, no_payload}, amilist:get_payload(AmiList1)).

get_payload_nonempty_list_payload_available_test() ->
    AmiList = amilist:new(),
    AmiList1 = amilist:set_payload(AmiList, "Payload"),
    ?assertEqual("Payload", amilist:get_payload(AmiList1)).

get_payload_multple_entries_list_payload_available_test() ->
    AmiList = amilist:new(),
    AmiList1 = amilist:set_value(AmiList, key1, "Value1"),
    AmiList2 = amilist:set_value(AmiList1, key2, "Value2"),
    AmiList3 = amilist:set_payload(AmiList2, "Payload"),
    ?assertEqual("Payload", amilist:get_payload(AmiList3)).

get_payload_multple_entries_list_nopayload_test() ->
    AmiList = amilist:new(),
    AmiList1 = amilist:set_value(AmiList, key1, "Value1"),
    AmiList2 = amilist:set_value(AmiList1, key2, "Value2"),
    AmiList3 = amilist:set_value(AmiList2, key3, "Value3"),
    ?assertEqual({error, no_payload}, amilist:get_payload(AmiList3)).



has_payload_empty_list_test() ->
    AmiList = amilist:new(),
    ?assertEqual(false, amilist:has_payload(AmiList)).

has_payload_nonempty_list_nopayload_test() ->
    AmiList = amilist:new(),
    AmiList1 = amilist:set_value(AmiList, key1, "Value1"),
    ?assertEqual(false, amilist:has_payload(AmiList1)).

has_payload_nonempty_list_payload_available_test() ->
    AmiList = amilist:new(),
    AmiList1 = amilist:set_payload(AmiList, "Payload"),
    ?assertEqual(true, amilist:has_payload(AmiList1)).

has_payload_multple_entries_list_payload_available_test() ->
    AmiList = amilist:new(),
    AmiList1 = amilist:set_value(AmiList, key1, "Value1"),
    AmiList2 = amilist:set_value(AmiList1, key2, "Value2"),
    AmiList3 = amilist:set_payload(AmiList2, "Payload"),
    ?assertEqual(true, amilist:has_payload(AmiList3)).

has_payload_multple_entries_list_nopayload_test() ->
    AmiList = amilist:new(),
    AmiList1 = amilist:set_value(AmiList, key1, "Value1"),
    AmiList2 = amilist:set_value(AmiList1, key2, "Value2"),
    AmiList3 = amilist:set_value(AmiList2, key3, "Value3"),
    ?assertEqual(false, amilist:has_payload(AmiList3)).

