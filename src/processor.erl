-module(processor).
-export([
        extract_packets/2
    ]).


-define(MARK, "PROCESSORMARK").

%% ====================================================================
%% Exported Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc
%% @spec extract_packets(String, Token) -> {ListOfStrings, RemainderString}
%% @end
%% --------------------------------------------------------------------

extract_packets(String, Token) ->
    MarkedString = append_mark(String),
    ListOfStrings = split(MarkedString, Token),
    extract_list_and_remainder(ListOfStrings).

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
    TotalLen = string:len(MarkedString),
    MarkerLen = string:len(?MARK),
    MarkerPosition = TotalLen - MarkerLen,
    string:substr(MarkedString, 1, MarkerPosition).


%% ---------------------------------------------------------------------
%% @doc
%% @spec split(String, Str) -> [ListOfStrings]
%% @end
%% ---------------------------------------------------------------------

split(String, Str) ->
    split(String, Str, []).



%% =====================================================================
%% Private Functions
%% =====================================================================

extract_list_and_remainder(ListOfStrings) ->
    extract_list_and_remainder(ListOfStrings, []).

extract_list_and_remainder([], Accm) ->
    {Accm, ""};
extract_list_and_remainder([String], Accm) ->
    {Accm, remove_mark(String)};
extract_list_and_remainder([H | T], Accm) ->
    extract_list_and_remainder(T, [H | Accm]).


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
