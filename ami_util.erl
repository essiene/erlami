-module(ami_util).
-author("Essien Ita Essien <essien.ita@uknglobal.com>").
-export([
        build_command/1,
        build_vars/1,
        parse_response/1,
        is_response_complete/1
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


build_vars(ListOfTuples) ->
    build_vars(ListOfTuples, []).

build_vars([{Key, Value} | T], Accm) ->
    V = string:concat(string:to_upper(atom_to_list(Key)), "="),
    V1 = string:concat(V, Value),
    build_vars(T, [{variable, V1} | Accm]);
build_vars([], Accm) ->
    Accm.


parse_response(Response) ->
    Stripped = string:strip(Response),
    Lines = string:tokens(Stripped, "\r\n"),
    lines_to_dict(Lines, dict:new()).

lines_to_dict([], Dict) -> Dict;
lines_to_dict([H | T], Dict) ->
    [Key | Rest] = string:tokens(H, ":"),
    LowerCaseKey = string:to_lower(Key),
    AtomKey = list_to_atom(LowerCaseKey),
    Value = string:strip(string:join(Rest, ":")),
    lines_to_dict(T, dict:store(AtomKey, Value, Dict)).



is_response_complete(Str) ->
    case string:substr(Str, string:len(Str) - 3) of
        "\r\n\r\n" ->
            true;
        _ ->
            false
    end.
