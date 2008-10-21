-module(ami).
-author("Essien Ita Essien <essien.ita@uknglobal.com>").
-export([
        new/2,
        new/4,
        execute/2,
        originate/5
    ]).



new(Username, Secret) ->
    new("localhost", 5038, Username, Secret).

new(Host, Port, Username, Secret) ->
    Client = amitcp:connect(Host, Port),
    amiclient_session:new(Client, Username, Secret).

originate(Interp, Channel, Context, Extension, Priority) ->
    execute(Interp, [
            {action, originate},
            {channel, Channel},
            {context, Context},
            {exten, Extension},
            {pRiority, Priority}
        ]).

execute(_Ami, []) ->
    {error, no_command_specified};
execute(Ami, [{action, _Action} | _Rest] = Command) ->
    interp:rpc(Ami, Command).
