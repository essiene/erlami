-module(ami_util).
-author("Essien Ita Essien <essien.ita@uknglobal.com>").
-export([
        build_command/1,
        parse_response/1,
        logmessage/1
    ]).

build_command(ListOfTuples) ->
    build_command(ListOfTuples, "").

build_command([{Key, Value} | T], Command) ->
    build_command(T, string:concat(Command, build_line(Key, Value)));
build_command([], Command) ->
    string:concat(Command, "\r\n").

build_line(LHS, RHS) ->
    S1 = string:concat(string:to_upper(atom_to_list(LHS)), ": "),
    S2 = string:concat(S1, RHS),
    string:concat(S2, "\r\n").


parse_response(Response) ->
    Stripped = string:strip(Response),
    Lines = string:tokens(Stripped, "\r\n"),
    lines_to_dict(Lines, dict:new(), "", false).

lines_to_dict([], Dict, "", _) -> Dict;
lines_to_dict([], Dict, ExtraMessage, _) -> dict:store(message, ExtraMessage, Dict);
lines_to_dict([H | T], Dict, ExtraMessage, GrabExtraFlag) ->
        case GrabExtraFlag of
            true ->
                NewExtraMessage = string:concat(ExtraMessage, H),
                lines_to_dict(T, Dict, NewExtraMessage, true);
            false ->
                StrippedLine = string:strip(H),
                case string:chr(StrippedLine, 32) of
                    0 -> 
                        NewExtraMessage = string:concat(ExtraMessage, H),
                        lines_to_dict(T, Dict, NewExtraMessage, true);
                    _ ->
                        [KeyWithColon | Rest] = string:tokens(StrippedLine, " "),
                        case string:chr(KeyWithColon, $:) of
                            0 -> 
                                NewExtraMessage = string:concat(ExtraMessage, H),
                                lines_to_dict(T, Dict, NewExtraMessage, true);
                            _ ->
                                [Key] = string:tokens(KeyWithColon, ":"),
                                LowerCaseKey = string:to_lower(Key),
                                AtomKey = list_to_atom(LowerCaseKey),
                                Value = string:strip(string:join(Rest, ":")),
                                lines_to_dict(T, dict:store(AtomKey, Value, Dict), ExtraMessage, false)
                        end
                end
        end.



logmessage([]) ->
    io:format("~n");
logmessage([{Key, Value} | T]) when is_list(Value) ->
    io:format("~p~n", [{Key, Value}]),
    logmessage(T);
logmessage(Any) ->
    io:format("~p~n", [Any]).
