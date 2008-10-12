-module(util).
-author("Essien Ita Essien <essien.ita@uknglobal.com>").
-export([
        split/2,
        append_mark/1,
        remove_mark/1,
        get_seperated_blocks_and_incomplete/1,
        logmessage/1
    ]).


-define(MARK, "PROCESSORMARK").

%% ---------------------------------------------------------------------
%% @doc
%% @spec split(String, Str) -> [ListOfStrings]
%% @end
%% ---------------------------------------------------------------------

split(String, Seperator) ->
    split(String, Seperator, []).

% Handles case split("", "SEP") -> []
split("", _, Accm) ->
    Accm;
% Handles case split("SEP", "SEP") -> []
split(Seperator, Seperator, Accm) ->
    Accm;
% Catches the bug inherent when "SEP" happens to 
% terminate the string, else, we'll always have
% an extra "" at the end of the resulting list
split(Tail, Seperator, ["" | Accm]) ->
    split(Tail, Seperator, Accm);
% The normal form
split(String, Seperator, Accm) ->
    SeperatorLen = string:len(Seperator),
    case string:rstr(String, Seperator) of
        0 ->
            [String | Accm];
        Index ->
            Head = string:substr(String, Index + SeperatorLen),
            Tail = string:substr(String, 1, Index - 1),
            split(Tail, Seperator, [Head | Accm])
    end.


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
%% @spec get_seperated_blocks_and_incomplete(RawListOfBlocks) ->
%%          {ListOfCompleteBlocks, IncompleteBlockString}
%% @end
%% ---------------------------------------------------------------------

get_seperated_blocks_and_incomplete(RawListOfString) ->
    {ListOfStrings, Incomplete} = get_seperated_blocks_and_incomplete(RawListOfString, ?MARK, []),
    {lists:reverse(ListOfStrings), Incomplete}.

get_seperated_blocks_and_incomplete([], _Mark, Accm) ->
    {Accm, ""};
get_seperated_blocks_and_incomplete([LastBlock], Mark, Accm) ->
    % implement util:startwith(String, Check)
    % and util:endswith(String, Check)
    case string:str(LastBlock, Mark) of
        0 ->
            {[LastBlock | Accm], ""};
        _ ->
            {Accm, remove_mark(LastBlock)}
    end;
get_seperated_blocks_and_incomplete([H | T], Mark, Accm) ->
    get_seperated_blocks_and_incomplete(T, Mark, [H | Accm]).

logmessage([]) ->
    io:format("~n");
logmessage([{Key, Value} | T]) ->
    io:format("~p~n", [{Key, Value}]),
    logmessage(T);
logmessage(Any) ->
    io:format("~p~n", [Any]).

