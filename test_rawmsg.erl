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
    ?assertEqual({["string1SEPstring2SEPstring3"], "string4SEPstring5SEPstring6"}, rawmsg:get_blocks("string1SEPstring2SEPstring3SEPSEPstring4SEPstring5SEPstring6", "SEPSEP")).

get_blocks_no_incomplete_test() ->
    ?assertEqual({["string1SEPstring2SEPstring3", "string4SEPstring5SEPstring6"], ""}, rawmsg:get_blocks("string1SEPstring2SEPstring3SEPSEPstring4SEPstring5SEPstring6SEPSEP", "SEPSEP")).
