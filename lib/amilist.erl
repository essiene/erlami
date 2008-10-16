-module(amilist).
-export([
        new/0,
        set_value/3,
        get_value/2,
        has_value/2,
        has_key/2,
        set_payload/2,
        get_payload/1,
        has_payload/1
    ]).


%% ==========================================================================
%%
%% Amilists are Key Value lists with one optional keyless value called the
%% 'payload'.  Amilists are used for messaging throughout the library.
%%
%% They are protocol aware.
%% A amilist is really a 2 tuple list of the form:
%%    [{Key1, Value1} | Rest]
%%
%% This structure is mapped one-to-one to the AMI Message Block format which looks
%% like:
%%
%%    Key1: Value1\r\n
%%    Key2: Value2\r\n
%%    ...
%%    KeyN: ValueN\r\n
%%    Optional payload without Key-Value mapping \r\n
%%    that can spawn multiple lines and ends with
%%    \r\n
%% 
%%  Because they are protocol aware, each time we meet an optional
%%  multiline payload, we read it into a keyless value
%%  and the end result becomes:
%%
%%  [
%%      {key1, "value1"},
%%      {key2, "value2"},
%%      ...
%%      {keyN, "valueN"},
%%      {"This particular tuple entry\r\n
%%        is completely optional\r\n"
%%      }
%%  ]
%%
%% This helps to keep the protocol consistent on the library end and obsoletes the
%% need for special case hacks.
%%
%% ==========================================================================




new() ->
    [].

set_value(AmiList, Key, Value) ->
    lists:keystore(Key, 1, AmiList, {Key, Value}).

get_value(AmiList, Key) ->
    case lists:keysearch(Key, 1, AmiList) of
        {value, {Key, Value}} ->
            Value;
        false ->
            {keyerror, Key}
    end.

has_value(AmiList, Value) ->
    lists:keymember(Value, 2, AmiList).



has_key(AmiList, Key) ->
    lists:keymember(Key, 1, AmiList).


set_payload(AmiList, Payload) ->
    set_payload(AmiList, [], Payload).

set_payload([], NewAmiList, Payload) ->
    lists:reverse([Payload | NewAmiList]);
set_payload([{Key, Value} | Rest], NewAmiList, Payload) ->
    set_payload(Rest, [{Key, Value} | NewAmiList], Payload);
set_payload([{_OldPayload} | Rest], NewAmiList, Payload) ->
    set_payload(Rest, NewAmiList, Payload).

get_payload([{_Key, _Value} | Rest]) ->
    get_payload(Rest);
get_payload([{Payload} | _Rest]) ->
    Payload.

has_payload([]) ->
    false;
has_payload([{_Key, _Value} | Rest]) ->
    has_payload(Rest);
has_payload([{_Payload} | _Rest]) ->
    true.
