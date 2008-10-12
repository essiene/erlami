-module(test_messaging).
-include_lib("eunit/include/eunit.hrl").

get_blocks_incomplete_test() ->
    ?assertEqual({["string1SEPstring2SEPstring3"], "string4SEPstring5SEPstring6"}, messaging:get_blocks("string1SEPstring2SEPstring3SEPSEPstring4SEPstring5SEPstring6", "SEPSEP")).

get_blocks_no_incomplete_test() ->
    ?assertEqual({["string1SEPstring2SEPstring3", "string4SEPstring5SEPstring6"], ""}, messaging:get_blocks("string1SEPstring2SEPstring3SEPSEPstring4SEPstring5SEPstring6SEPSEP", "SEPSEP")).
