-module(messaging).
-export([
        get_blocks/2,
        get_blocks/1,
        amilist_to_block/1, 
        block_to_amilist/1,
        send/2
    ]).
-include("ami.hrl").

%% ====================================================================
%% Exported Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc
%% @spec get_blocks(RawString, BlockSeperator) -> 
%%   {BlockList, TerminatingIncompleteBlock}
%% 
%% This function takes a raw string which could be made up of multiple
%% message blocks, and tries to intelligently seperate them into 
%% the seperate message blocks. Each time it finds a complete message block,
%% It adds it as a distinct entry in a list of complete blocks. 
%%
%% Sometimes incomplete blocks are found at the end of the raw string. 
%% These are returned as the second object in the result tuple.
%% 
%% The onus is on the user to prepend this remainder to new incoming data
%% before attempting to get blocks again.
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
    get_blocks(RawListOfBlocks).

get_blocks(RawListOfString) ->
    {ListOfStrings, Incomplete} = get_blocks(RawListOfString, ?MARK, []),
    {lists:reverse(ListOfStrings), Incomplete}.

get_blocks([], _Mark, BlockList) ->
    {BlockList, ""};
get_blocks([LastBlock], Mark, BlockList) ->
    % implement util:startwith(String, Check)
    % and util:endswith(String, Check)
    case string:str(LastBlock, Mark) of
        0 ->
            {[LastBlock | BlockList], ""};
        _ ->
            {BlockList, util:remove_mark(LastBlock)}
    end;
get_blocks([H | T], Mark, BlockList) ->
    get_blocks(T, Mark, [H | BlockList]).


%% ---------------------------------------------------------------------------
%% @doc
%% @spec amilist_to_block(AMIList) -> AMIBlock
%% @spec amilist_to_block(AMIList, BlockAccumulator, Payload) -> AMIBlock
%% @end
%% ------------------------------------------------------------------------

amilist_to_block(AMIList) ->
    amilist_to_block(AMIList, "", "").


amilist_to_block([{Value} | T], Block, Payload) when is_list(Value) ->
    NewPayload = string:concat(Payload, string:concat(Value, "\r\n")),
    amilist_to_block(T, Block, NewPayload);
amilist_to_block([{Key, Value} | T], Block, Payload) ->
    amilist_to_block(T, string:concat(Block, create_line(Key, Value)), Payload);
amilist_to_block([], Block, "") ->
    string:concat(Block, "\r\n");
amilist_to_block([], Block, Payload) ->
    C1 = string:concat(Block, Payload),
    string:concat(C1, "\r\n").

%% ---------------------------------------------------------------------------
%% @doc
%% @spec block_to_amilist(AMIList) -> AMIBlock
%% @end doc
%% ---------------------------------------------------------------------------

block_to_amilist(Block) ->
    Stripped = util:strip(Block),
    ListOfLines = string:tokens(Stripped, "\r\n"),
    lists:reverse(lineslist_to_amilist(ListOfLines, [], "", false)).


send(Socket, Data) ->
    case gen_tcp:send(Socket, Data) of
        {error, Reason} ->
            throw({send, Reason});
        ok ->
            {ok, Data}
    end.




%% --------------------------------------------------------------------------
%% @doc
%% @spec lineslist_to_amilist(ListOfLines, AMIListAccumulator, Payload, GrabPayloadFlag) -> AMIList
%% @end
%% --------------------------------------------------------------------------

lineslist_to_amilist([], AMIList, "", _) -> AMIList;
lineslist_to_amilist([], AMIList, Payload, _) -> [{util:strip(Payload)} | AMIList]; 
lineslist_to_amilist([H | T], AMIList, Payload, GrabPayloadFlag) ->
        case GrabPayloadFlag of
            true ->
                NewPayload = string:concat(Payload, string:concat(H,"\r\n")),
                lineslist_to_amilist(T, AMIList, NewPayload, true);
            false ->
                StrippedLine = util:strip(H),
                case string:chr(StrippedLine, 32) of
                    0 -> 
                        NewPayload = string:concat(Payload, string:concat(H,"\r\n")),
                        lineslist_to_amilist(T, AMIList, NewPayload, true);
                    _ ->
                        [KeyWithColon | Rest] = string:tokens(StrippedLine, " "),
                        case string:chr(KeyWithColon, $:) of
                            0 -> 
                                NewPayload = string:concat(Payload, string:concat(H, "\r\n")),
                                lineslist_to_amilist(T, AMIList, NewPayload, true);
                            _ColonIndex ->
                                [Key] = string:tokens(KeyWithColon, ":"),
                                LowerCaseKey = string:to_lower(Key),
                                AtomKey = list_to_atom(LowerCaseKey),
                                Value = string:join(Rest, " "),
                                lineslist_to_amilist(T, [{AtomKey, Value} | AMIList], Payload, false)
                        end
                end
        end.


%% ---------------------------------------------------------------------------
%% @doc
%% @spec create_line(lhs, RHS) -> "LHS: RHS\r\n"
%% @end
%% ---------------------------------------------------------------------------

create_line(LHS, RHS) when is_atom(LHS), is_list(RHS) ->
    S1 = string:concat(string:to_upper(atom_to_list(LHS)), ": "),
    S2 = string:concat(S1, RHS),
    string:concat(S2, "\r\n").
