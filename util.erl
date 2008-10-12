-module(util).
-author("Essien Ita Essien <essien.ita@uknglobal.com>").
-export([
        split/2,
        logmessage/1
    ]).

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


logmessage([]) ->
    io:format("~n");
logmessage([{Key, Value} | T]) when is_list(Value) ->
    io:format("~p~n", [{Key, Value}]),
    logmessage(T);
logmessage(Any) ->
    io:format("~p~n", [Any]).
