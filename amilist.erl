-module(amilist).
-export([
        to_block/1,
        from_lines/1,
        from_block/1,
        has_key/2,
        has_value/2,
        get_value/2,
        set_value/3
    ]).


%% ==========================================================================
%%
%% amilists are Key Value lists which used for messaging throughout the library.
%% The are protocol aware.
%% A amilist is really a 2 tuple list of the form:
%%    [{Key1, Value1} | Rest]
%%
%% This structure is mapped one-to-one to the AMI message format which looks
%% like:
%%
%%    Key1: Value1\r\n
%%    Key2: Value2\r\n
%%    ...
%%    KeyN: ValueN\r\n
%%    Optional payload without Key-Value mapping \r\n
%%    that can spawn multiple lines and ends with
%%    --END COMMAND--\r\n
%%    \r\n
%% 
%%  Because they are protocol aware, each time we meet an optional
%%  multiline payload, we read it as if it had been a:
%%
%%     Message: Multiline Values
%%
%% This helps to keep the protocol consistent on the library end and obsoletes the
%% need for special case hacks.
%%
%% ==========================================================================


%% ---------------------------------------------------------------------------
%% @doc
%% @spec to_block(ListOfTuples) -> AMIBlock
%% @end doc
%% ---------------------------------------------------------------------------


to_block(ListOfTuples) ->
    to_block(ListOfTuples, "").

to_block([{Key, Value} | T], Command) ->
    to_block(T, string:concat(Command, create_line(Key, Value)));
to_block([], Command) ->
    string:concat(Command, "\r\n").

%% ---------------------------------------------------------------------------
%% @doc
%% @spec create_line(lhs, RHS) -> "LHS: RHS\r\n"
%% @end
%% ---------------------------------------------------------------------------

create_line(LHS, RHS) when is_atom(LHS), is_list(RHS) ->
    S1 = string:concat(string:to_upper(atom_to_list(LHS)), ": "),
    S2 = string:concat(S1, RHS),
    string:concat(S2, "\r\n").

from_block(Block) ->
    Stripped = util:strip(Block),
    ListOfLines = string:tokens(Stripped, "\r\n"),
    from_lines(ListOfLines).


has_key(AmiList, Key) ->
    lists:keymember(Key, 1, AmiList).

has_value(AmiList, Value) ->
    lists:keymember(Value, 2, AmiList).

get_value(AmiList, Key) ->
    case lists:keysearch(Key, 1, AmiList) of
        {ok, {Key, Value}} ->
            Value;
        false ->
            throw({keyerror, Key})
    end.

set_value(AmiList, Key, Value) ->
    lists:keystore(Key, 1, AmiList, {Key, Value}).

%% --------------------------------------------------------------------------
%% @doc
%% @spec from_lines(ListOfLines) -> ListOfTuples
%% @end
%% --------------------------------------------------------------------------

from_lines(ListOfLines) ->
    lists:reverse(from_lines(ListOfLines, [], "", false)).


from_lines([], TupleList, "", _) -> TupleList;
from_lines([], TupleList, ExtraMessage, _) -> [{message, util:strip(ExtraMessage)} | TupleList]; 
from_lines([H | T], TupleList, ExtraMessage, GrabExtraFlag) ->
        case GrabExtraFlag of
            true ->
                NewExtraMessage = string:concat(ExtraMessage, H),
                from_lines(T, TupleList, NewExtraMessage, true);
            false ->
                StrippedLine = util:strip(H),
                case string:chr(StrippedLine, 32) of
                    0 -> 
                        NewExtraMessage = string:concat(ExtraMessage, H),
                        from_lines(T, TupleList, NewExtraMessage, true);
                    _ ->
                        [KeyWithColon | Rest] = string:tokens(StrippedLine, " "),
                        case string:chr(KeyWithColon, $:) of
                            0 -> 
                                NewExtraMessage = string:concat(ExtraMessage, H),
                                from_lines(T, TupleList, NewExtraMessage, true);
                            _ColonIndex ->
                                [Key] = string:tokens(KeyWithColon, ":"),
                                LowerCaseKey = string:to_lower(Key),
                                AtomKey = list_to_atom(LowerCaseKey),
                                Value = string:join(Rest, " "),
                                from_lines(T, [{AtomKey, Value} | TupleList], ExtraMessage, false)
                        end
                end
        end.
