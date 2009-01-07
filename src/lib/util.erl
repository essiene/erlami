-module(util).
-author("Essien Ita Essien <essien.ita@uknglobal.com>").
-export([
        split/2,
        lstrip/1,
        rstrip/1,
        strip/1,
        append_mark/1,
        remove_mark/1,
        build_chan_vars/1,
        logmessage/1,
        proplists_remove/2,
        proplists_remove/3
    ]).

-include("ami.hrl").


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
%% @spec lstrip(String) -> StringWithoutLeftMostWhiteSpaces
%% @end
%% --------------------------------------------------------------------

lstrip("") ->
    "";
lstrip([H | RestOfString]) -> 
    case lists:member(H, [32, $\t, $\r, $\n]) of
        false ->
            [H | RestOfString];
        true ->
            lstrip(RestOfString)
    end.


rstrip("") ->
    "";
rstrip(String) ->
    S1 = lists:reverse(String),
    S2 = lstrip(S1),
    lists:reverse(S2).

strip("") ->
    "";
strip(String) ->
    S1 = lists:reverse(String),
    S2 = lstrip(S1),
    S3 = lists:reverse(S2),
    lstrip(S3).


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


build_chan_vars(ChanVars) ->
    build_chan_vars(ChanVars, []).

build_chan_vars([], Accm) ->
    lists:reverse(Accm);
build_chan_vars([{Key, Val} | Rest], Accm) when is_list(Key), is_list(Val) ->
    Var = string:join([Key, Val], "="),
    NewAccm = [{variable, Var} | Accm],
    build_chan_vars(Rest, NewAccm);
build_chan_vars([{Key, Val} | Rest], Accm) when is_atom(Key), is_list(Val) ->
    NewKey = atom_to_list(Key),
    build_chan_vars([{NewKey, Val} | Rest], Accm).
    


proplists_remove(List0, Key, Default) ->
    try
        proplists_remove(List0, Key)
    catch
        throw:{not_found, Key} ->
            List1 = proplists:delete(Key, List0),
            {List1, Default}
    end.


proplists_remove(List0, Key) ->
    case proplists:lookup(Key, List0) of
        none ->
            throw({not_found, Key});
        {Key, Val} ->
            List1 = proplists:delete(Key, List0),
            {List1, Val}
    end.


%% -------------------------------------------------------------------
%% @doc
%% @spec 
%% @end
%% -------------------------------------------------------------------

logmessage([]) ->
    void;
logmessage(Any) ->
    error_logger:info_report(Any).
