-module(test_amilist).
-include_lib("eunit/include/eunit.hrl").


to_block_none_test() ->
    ?assertEqual("\r\n", amilist:to_block([])).

to_block_no_payload_single_pair() ->
    TupleList = [{key1, "Value2"}],
    Block = "KEY1: Value1\r\n\r\n",
    ?assertEqual(Block, amilist:to_block(TupleList)).

to_block_no_payload_test() ->
    TupleList = [{key1, "Value1"}, {key2, "A longer Value2"}, {key3, "Value3"}],
    Block = "KEY1: Value1\r\nKEY2: A longer Value2\r\nKEY3: Value3\r\n\r\n",
    ?assertEqual(Block, amilist:to_block(TupleList)).

to_block_no_pair_single_payload_test() ->
    TupleList = [{"A payload line"}],
    Block = "A payload line\r\n--END COMMAND--\r\n\r\n",
    ?assertEqual(Block, amilist:to_block(TupleList)).

to_block_no_pair_multiple_payload_test() ->
    TupleList = [{"Payload line1"}, {"Payload line2"}, {"Payload line3"}],
    Block = "Payload line1\r\nPayload line2\r\nPayload line3\r\n--END COMMAND--\r\n\r\n",
    ?assertEqual(Block, amilist:to_block(TupleList)).

to_block_single_pair_single_payload_test() ->
    TupleList = [{key1, "Value one"}, {"A payload line"}],
    Block = "KEY1: Value one\r\nA payload line\r\n--END COMMAND--\r\n\r\n",
    ?assertEqual(Block, amilist:to_block(TupleList)).

to_block_single_pair_multiple_payload_test() ->
    TupleList = [{key1, "Value one"}, {"Payload line1"}, {"Payload line2"}, {"Payload line3"}],
    Block = "KEY1: Value one\r\nPayload line1\r\nPayload line2\r\nPayload line3\r\n--END COMMAND--\r\n\r\n",
    ?assertEqual(Block, amilist:to_block(TupleList)).

to_block_multiple_pair_multiple_payload_test() ->
    TupleList = [{key1, "Value one"}, {key2, "Value two"}, {key3, "Value three"}, {"Payload line1"}, {"Payload line2"}, {"Payload line3"}],
    Block = "KEY1: Value one\r\nKEY2: Value two\r\nKEY3: Value three\r\nPayload line1\r\nPayload line2\r\nPayload line3\r\n--END COMMAND--\r\n\r\n",
    ?assertEqual(Block, amilist:to_block(TupleList)).


from_lines_no_lines_test() ->
    ?assertEqual([], amilist:from_lines([])).

from_lines_no_payload_test() ->
    % Note that the space b/w Key: and Value is expected as part of the protocol
    % eventually we should use yecc or something to make a more forgiving grammar
    Lines = ["Key1: Value1\r\n", "Key2: A longer Value2\r\n", "Key3: Value3\r\n"],
    TupleList = [{key1, "Value1"}, {key2, "A longer Value2"}, {key3, "Value3"}],
    ?assertEqual(TupleList, amilist:from_lines(Lines)).

from_lines_single_line_payload_test() ->
    Lines = ["Key1: Value1\r\n", "Key2: A longer Value2\r\n", "--END COMMAND--\r\n"],
    TupleList = [{key1, "Value1"}, {key2, "A longer Value2"}, {message, "--END COMMAND--"}],
    ?assertEqual(TupleList, amilist:from_lines(Lines)).

from_lines_multi_line_payload_test() ->
    Lines = ["Key1: Value1\r\n", "Key2: A longer Value2\r\n", "Line one\r\nLine two\r\n--END COMMAND--\r\n"],
    TupleList = [{key1, "Value1"}, {key2, "A longer Value2"}, {message, "Line one\r\nLine two\r\n--END COMMAND--"}],
    ?assertEqual(TupleList, amilist:from_lines(Lines)).
