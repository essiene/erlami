-module(protocol).
-export([
    ]).


%lines_to_dict([], Dict, "", _) -> Dict;
%lines_to_dict([], Dict, ExtraMessage, _) -> dict:store(message, ExtraMessage, Dict);
%lines_to_dict([H | T], Dict, ExtraMessage, GrabExtraFlag) ->
%        case GrabExtraFlag of
%            true ->
%                NewExtraMessage = string:concat(ExtraMessage, H),
%                lines_to_dict(T, Dict, NewExtraMessage, true);
%            false ->
%                StrippedLine = string:strip(H),
%                case string:chr(StrippedLine, 32) of
%                    0 -> 
%                        NewExtraMessage = string:concat(ExtraMessage, H),
%                        lines_to_dict(T, Dict, NewExtraMessage, true);
%                    _ ->
%                        [KeyWithColon | Rest] = string:tokens(StrippedLine, " "),
%                        case string:chr(KeyWithColon, $:) of
%                            0 -> 
%                                NewExtraMessage = string:concat(ExtraMessage, H),
%                                lines_to_dict(T, Dict, NewExtraMessage, true);
%                            _ ->
%                                [Key] = string:tokens(KeyWithColon, ":"),
%                                LowerCaseKey = string:to_lower(Key),
%                                AtomKey = list_to_atom(LowerCaseKey),
%                                Value = string:strip(string:join(Rest, ":")),
%                                lines_to_dict(T, dict:store(AtomKey, Value, Dict), ExtraMessage, false)
%                        end
%                end
%        end.
