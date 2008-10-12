-module(messaging).
-export([
        get_blocks/2,
        request_build/1,
        response_parse/1
    ]).


%% ====================================================================
%% Exported Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc
%% @spec get_blocks(RawString, BlockSeperator) -> {ListOfBlocks, TerminatingIncompleteBlock}
%% 
%% This function takes a raw string which could be made up of multiple
%% message blocks, and tries to intelligently seperate them into 
%% the various message blocks. Each time it gets a complete message block,
%% It adds it as a distinct entry in a list of complete blocks. 
%%
%% Sometimes incomplete blocks are recieved. These will be held untill the
%% completing part of the block is recieved. See get_seperated_blocks_and_incomplete/1
%%
%% 1. We first terminate the recieved string with our own custom MARK.
%% 2. We split the terminated string on 'Token' according to the protocol
%%    into a list of blocks the final block being potentially incomplete.
%% 3. We then pass the list to get_blocks/1 which will
%%    return the complete blocks which we already have gotten, and a 
%%    possible incomplete block returned as the remainder.
%%    
%% @end
%% --------------------------------------------------------------------

get_blocks(RawString, BlockSeperator) ->
    MarkedString = util:append_mark(RawString),
    RawListOfBlocks = util:split(MarkedString, BlockSeperator),
    util:get_seperated_blocks_and_incomplete(RawListOfBlocks).

request_build(ListOfTuples) ->
    amilist:to_lines(ListOfTuples).

response_parse(Response) ->
    Stripped = string:strip(Response),
    Lines = string:tokens(Stripped, "\r\n"),
    amilist:from_lines(Lines).

%% =====================================================================
%% Private Functions
%% =====================================================================

