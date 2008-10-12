-module(test_amilist).
-include_lib("eunit/include/eunit.hrl").


to_block_none_test() ->
    ?assertEqual("\r\n", amilist:to_block([])).

to_block_test() ->
    TupleList = [{key1, "Value1"}, {key2, "A longer Value2"}, {key3, "Value3"}],
    Block = "KEY1: Value1\r\nKEY2: A longer Value2\r\nKEY3: Value3\r\n\r\n",
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
