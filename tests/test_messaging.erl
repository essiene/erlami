-module(test_messaging).
-include_lib("eunit/include/eunit.hrl").

get_blocks_incomplete_test() ->
    ?assertEqual({["string1SEPstring2SEPstring3"], "string4SEPstring5SEPstring6"}, messaging:get_blocks("string1SEPstring2SEPstring3SEPSEPstring4SEPstring5SEPstring6", "SEPSEP")).

get_blocks_no_incomplete_test() ->
    ?assertEqual({["string1SEPstring2SEPstring3", "string4SEPstring5SEPstring6"], ""}, messaging:get_blocks("string1SEPstring2SEPstring3SEPSEPstring4SEPstring5SEPstring6SEPSEP", "SEPSEP")).




get_blocks_empty_list_test() ->
    ?assertEqual({[], ""}, messaging:get_blocks([])).

get_blocks_and_single_test() ->
    ?assertEqual({["string1"], ""}, messaging:get_blocks(["string1"])).

get_blocks_and_no_incomplete_test() ->
    ?assertEqual({["string1", "string2"], ""}, messaging:get_blocks(["string1", "string2"])).

get_blocks_test() ->
    ?assertEqual({["string1"], "string2"}, messaging:get_blocks(["string1", "string2PROCESSORMARK"])).

get_blocks_and_multiple_no_incomplete_test() ->
    ?assertEqual({["string1", "string2", "string3", "string4"], ""}, messaging:get_blocks(["string1", "string2", "string3", "string4"])).

get_blocks_multiple_and_incomplete_test() ->
    ?assertEqual({["string1", "string2", "string3"], "string4"}, messaging:get_blocks(["string1", "string2", "string3", "string4PROCESSORMARK"])).





amilist_to_block_none_test() ->
    ?assertEqual("\r\n", messaging:amilist_to_block([])).

amilist_to_block_no_payload_single_pair_test() ->
    AMIList = [{key1, "Value1"}],
    Block = "KEY1: Value1\r\n\r\n",
    ?assertEqual(Block, messaging:amilist_to_block(AMIList)).

amilist_to_block_no_payload_test() ->
    AMIList = [{key1, "Value1"}, {key2, "A longer Value2"}, {key3, "Value3"}],
    Block = "KEY1: Value1\r\nKEY2: A longer Value2\r\nKEY3: Value3\r\n\r\n",
    ?assertEqual(Block, messaging:amilist_to_block(AMIList)).

amilist_to_block_no_pair_singleline_payload_test() ->
    AMIList = [{"A payload line"}],
    Block = "A payload line\r\n\r\n",
    ?assertEqual(Block, messaging:amilist_to_block(AMIList)).

amilist_to_block_no_pair_multiline_payload_test() ->
    AMIList = [{"Payload line1\r\nPayload line2\r\nPayload line3"}],
    Block = "Payload line1\r\nPayload line2\r\nPayload line3\r\n\r\n",
    ?assertEqual(Block, messaging:amilist_to_block(AMIList)).

amilist_to_block_single_pair_singleline_payload_test() ->
    AMIList = [{key1, "Value one"}, {"A payload line"}],
    Block = "KEY1: Value one\r\nA payload line\r\n\r\n",
    ?assertEqual(Block, messaging:amilist_to_block(AMIList)).

amilist_to_block_single_pair_multiline_payload_test() ->
    AMIList = [{key1, "Value one"}, {"Payload line1\r\nPayload line2\r\nPayload line3"}],
    Block = "KEY1: Value one\r\nPayload line1\r\nPayload line2\r\nPayload line3\r\n\r\n",
    ?assertEqual(Block, messaging:amilist_to_block(AMIList)).

amilist_to_block_multiple_pair_multiline_payload_test() ->
    AMIList = [
        {key1, "Value one"}, 
        {key2, "Value two"}, 
        {key3, "Value three"}, 
        {"Payload line1\r\nPayload line2\r\nPayload line3"}
    ],

    Block = "KEY1: Value one\r\nKEY2: Value two\r\nKEY3: Value three\r\nPayload line1\r\nPayload line2\r\nPayload line3\r\n\r\n",
    ?assertEqual(Block, messaging:amilist_to_block(AMIList)).





block_to_amilist_none_test() ->
    ?assertEqual([], messaging:block_to_amilist("\r\n")).

block_to_amilist_no_payload_single_pair_test() ->
    AMIList = [{key1, "Value1"}],
    Block = "KEY1: Value1\r\n\r\n",
    ?assertEqual(AMIList, messaging:block_to_amilist(Block)).

block_to_amilist_no_payload_test() ->
    AMIList = [{key1, "Value1"}, {key2, "A longer Value2"}, {key3, "Value3"}],
    Block = "KEY1: Value1\r\nKEY2: A longer Value2\r\nKEY3: Value3\r\n\r\n",
    ?assertEqual(AMIList, messaging:block_to_amilist(Block)).

block_to_amilist_no_pair_singleline_payload_test() ->
    AMIList = [{"A payload line"}],
    Block = "A payload line\r\n\r\n",
    ?assertEqual(AMIList, messaging:block_to_amilist(Block)).

block_to_amilist_no_pair_multiline_payload_test() ->
    AMIList = [{"Payload line1\r\nPayload line2\r\nPayload line3"}],
    Block = "Payload line1\r\nPayload line2\r\nPayload line3\r\n\r\n",
    ?assertEqual(AMIList, messaging:block_to_amilist(Block)).

block_to_amilist_single_pair_singleline_payload_test() ->
    AMIList = [{key1, "Value one"}, {"A payload line"}],
    Block = "KEY1: Value one\r\nA payload line\r\n\r\n",
    ?assertEqual(AMIList, messaging:block_to_amilist(Block)).

block_to_amilist_single_pair_multiline_payload_test() ->
    AMIList = [{key1, "Value one"}, {"Payload line1\r\nPayload line2\r\nPayload line3"}],
    Block = "KEY1: Value one\r\nPayload line1\r\nPayload line2\r\nPayload line3\r\n\r\n",
    ?assertEqual(AMIList, messaging:block_to_amilist(Block)).

block_to_amilist_multiple_pair_multiline_payload_test() ->
    AMIList = [
        {key1, "Value one"}, 
        {key2, "Value two"}, 
        {key3, "Value three"}, 
        {"Payload line1\r\nPayload line2\r\nPayload line3"}
    ],

    Block = "KEY1: Value one\r\nKEY2: Value two\r\nKEY3: Value three\r\nPayload line1\r\nPayload line2\r\nPayload line3\r\n\r\n",
    ?assertEqual(AMIList, messaging:block_to_amilist(Block)).






symmetry_none_test() ->
    ?assertEqual("\r\n", messaging:amilist_to_block(messaging:block_to_amilist("\r\n"))).

symmetry_none1_test() ->
    ?assertEqual([], messaging:block_to_amilist(messaging:amilist_to_block([]))).

symmetry_no_payload_single_pair_test() ->
    AMIList = [{key1, "Value1"}],
    ?assertEqual(AMIList, messaging:block_to_amilist(messaging:amilist_to_block(AMIList))).

symmetry_no_payload_single_pair1_test() ->
    Block = "KEY1: Value1\r\n\r\n",
    ?assertEqual(Block, messaging:amilist_to_block(messaging:block_to_amilist(Block))).

symmetry_no_payload_test() ->
    AMIList = [{key1, "Value1"}, {key2, "A longer Value2"}, {key3, "Value3"}],
    ?assertEqual(AMIList, messaging:block_to_amilist(messaging:amilist_to_block(AMIList))).

symmetry_no_payload1_test() ->
    Block = "KEY1: Value1\r\nKEY2: A longer Value2\r\nKEY3: Value3\r\n\r\n",
    ?assertEqual(Block, messaging:amilist_to_block(messaging:block_to_amilist(Block))).

symmetry_no_pair_singleline_payload_test() ->
    AMIList = [{"A payload line"}],
    ?assertEqual(AMIList, messaging:block_to_amilist(messaging:amilist_to_block(AMIList))).

symmetry_no_pair_singleline_payload1_test() ->
    Block = "A payload line\r\n\r\n",
    ?assertEqual(Block, messaging:amilist_to_block(messaging:block_to_amilist(Block))).

symmetry_no_pair_multiline_payload_test() ->
    AMIList = [{"Payload line1\r\nPayload line2\r\nPayload line3"}],
    ?assertEqual(AMIList, messaging:block_to_amilist(messaging:amilist_to_block(AMIList))).

symmetry_no_pair_multiline_payload1_test() ->
    Block = "Payload line1\r\nPayload line2\r\nPayload line3\r\n\r\n",
    ?assertEqual(Block, messaging:amilist_to_block(messaging:block_to_amilist(Block))).

symmetry_single_pair_singleline_payload_test() ->
    AMIList = [{key1, "Value one"}, {"A payload line"}],
    ?assertEqual(AMIList, messaging:block_to_amilist(messaging:amilist_to_block(AMIList))).

symmetry_single_pair_singleline_payload1_test() ->
    Block = "KEY1: Value one\r\nA payload line\r\n\r\n",
    ?assertEqual(Block, messaging:amilist_to_block(messaging:block_to_amilist(Block))).

symmetry_single_pair_multiline_payload_test() ->
    AMIList = [{key1, "Value one"}, {"Payload line1\r\nPayload line2\r\nPayload line3"}],
    ?assertEqual(AMIList, messaging:block_to_amilist(messaging:amilist_to_block(AMIList))).

symmetry_single_pair_multiline_payload1_test() ->
    Block = "KEY1: Value one\r\nPayload line1\r\nPayload line2\r\nPayload line3\r\n\r\n",
    ?assertEqual(Block, messaging:amilist_to_block(messaging:block_to_amilist(Block))).

symmetry_multiple_pair_multiline_payload_test() ->
    AMIList = [
        {key1, "Value one"}, 
        {key2, "Value two"}, 
        {key3, "Value three"}, 
        {"Payload line1\r\nPayload line2\r\nPayload line3"}
    ],
    ?assertEqual(AMIList, messaging:block_to_amilist(messaging:amilist_to_block(AMIList))).

symmetry_multiple_pair_multiline_payload1_test() ->
    Block = "KEY1: Value one\r\nKEY2: Value two\r\nKEY3: Value three\r\nPayload line1\r\nPayload line2\r\nPayload line3\r\n\r\n",
    ?assertEqual(Block, messaging:amilist_to_block(messaging:block_to_amilist(Block))).



lineslist_to_amilist_no_lines_test() ->
    ?assertEqual([], messaging:lineslist_to_amilist([])).

lineslist_to_amilist_no_payload_test() ->
    % Note that the space b/w Key: and Value is expected as part of the protocol
    % eventually we should use yecc or something to make a more forgiving grammar
    Lines = ["Key1: Value1\r\n", "Key2: A longer Value2\r\n", "Key3: Value3\r\n"],
    AmiList = [{key1, "Value1"}, {key2, "A longer Value2"}, {key3, "Value3"}],
    ?assertEqual(AmiList, messaging:lineslist_to_amilist(Lines)).

lineslist_to_amilist_single_line_payload_test() ->
    Lines = ["Key1: Value1\r\n", "Key2: A longer Value2\r\n", "--END COMMAND--\r\n"],
    AmiList = [{key1, "Value1"}, {key2, "A longer Value2"}, {"--END COMMAND--"}],
    ?assertEqual(AmiList, messaging:lineslist_to_amilist(Lines)).

lineslist_to_amilist_multi_line_payload_test() ->
    Lines = ["Key1: Value1\r\n", "Key2: A longer Value2\r\n", "Line one\r\nLine two\r\n--END COMMAND--\r\n"],
    AmiList = [{key1, "Value1"}, {key2, "A longer Value2"}, {"Line one\r\nLine two\r\n--END COMMAND--"}],
    ?assertEqual(AmiList, messaging:lineslist_to_amilist(Lines)).
