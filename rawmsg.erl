-module(rawmsg).
-export([
        get_blocks/2,
        append_mark/1,
        remove_mark/1,
        split/2,
        get_blocks_and_incomplete/1
    ]).


-define(MARK, "PROCESSORMARK").

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
%% completing part of the block is recieved. See get_blocks_and_incomplete/1
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
    MarkedString = append_mark(RawString),
    RawListOfBlocks = split(MarkedString, BlockSeperator),
    get_blocks_and_incomplete(RawListOfBlocks).

%% =====================================================================
%% Private Functions
%% =====================================================================
%% --------------------------------------------------------------------
%% @doc
%% @spec append_mark(String) -> MarkedString | ""
%% @end
%% --------------------------------------------------------------------

append_mark("") -> "";
append_mark(String) ->
    string:concat(String, ?MARK).


%% --------------------------------------------------------------------
%% @doc 
%% @spec remove_mark(MarkedString) -> String | ""
%% @end
%% --------------------------------------------------------------------

remove_mark("") -> "";
remove_mark(MarkedString) ->
    case string:str(MarkedString, ?MARK) of
        0 ->
            MarkedString;
        _ ->
            TotalLen = string:len(MarkedString),
            MarkerLen = string:len(?MARK),
            MarkerPosition = TotalLen - MarkerLen,
            string:substr(MarkedString, 1, MarkerPosition)
    end.


%% ---------------------------------------------------------------------
%% @doc
%% @spec split(String, Str) -> [ListOfStrings]
%% @end
%% ---------------------------------------------------------------------

split(String, Str) ->
    split(String, Str, []).

split(String, Str, Accm) ->
    SeperatorLen = string:len(Str),
    case string:rstr(String, Str) of
        0 ->
            [String | Accm];
        Index ->
            Head = string:substr(String, Index + SeperatorLen),
            Tail = string:substr(String, 1, Index - 1),
            split(Tail, Str, [Head | Accm])
    end.


%% ---------------------------------------------------------------------
%% @doc
%% @spec get_blocks_and_incomplete(RawListOfBlocks) ->
%%          {ListOfCompleteBlocks, IncompleteBlockString}
%% @end
%% ---------------------------------------------------------------------

get_blocks_and_incomplete(RawListOfString) ->
    get_blocks_and_incomplete(RawListOfString, []).

get_blocks_and_incomplete([], Accm) ->
    {Accm, ""};
get_blocks_and_incomplete([LastBlock], Accm) ->
    {Accm, remove_mark(LastBlock)};
get_blocks_and_incomplete([H | T], Accm) ->
    get_blocks(T, [H | Accm]).

