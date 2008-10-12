-module(rawmsg).
-export([
        get_blocks/2,
        append_mark/1,
        remove_mark/1,
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
    RawListOfBlocks = util:split(MarkedString, BlockSeperator),
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
%% @spec get_blocks_and_incomplete(RawListOfBlocks) ->
%%          {ListOfCompleteBlocks, IncompleteBlockString}
%% @end
%% ---------------------------------------------------------------------

get_blocks_and_incomplete(RawListOfString) ->
    {ListOfStrings, Incomplete} = get_blocks_and_incomplete(RawListOfString, ?MARK, []),
    {lists:reverse(ListOfStrings), Incomplete}.

get_blocks_and_incomplete([], _Mark, Accm) ->
    {Accm, ""};
get_blocks_and_incomplete([LastBlock], Mark, Accm) ->
    % implement util:startwith(String, Check)
    % and util:endswith(String, Check)
    case string:str(LastBlock, Mark) of
        0 ->
            {[LastBlock | Accm], ""};
        _ ->
            {Accm, remove_mark(LastBlock)}
    end;
get_blocks_and_incomplete([H | T], Mark, Accm) ->
    get_blocks_and_incomplete(T, Mark, [H | Accm]).
