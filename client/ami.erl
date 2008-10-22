-module(ami).
-author("Essien Ita Essien <essien.ita@uknglobal.com>").
-export([
        new/2,
        new/4,
        execute/2,
        originate/6
    ]).



new(Username, Secret) ->
    new("localhost", 5038, Username, Secret).

new(Host, Port, Username, Secret) ->
    Client = amitcp:connect(Host, Port),
    amiclient_session:new(Client, Username, Secret).

originate(Interp, Channel, _Number, Context, Extension, Priority) ->
    DialedChannel = Channel, %string:join([Channel, Number], "/"),
    execute(Interp, [
            {action, originate},
            {channel, DialedChannel},
            {context, Context},
            {exten, Extension},
            {'priority', Priority},
            {account, Channel}
        ]).

execute(_Ami, []) ->
    {error, no_command_specified};

execute(Ami, [{action, _Action} | _Rest] = Command) ->
    interp:rpc(Ami, Command).
