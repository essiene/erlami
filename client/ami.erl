-module(ami).
-author("Essien Ita Essien <essien.ita@uknglobal.com>").
-export([
        new/2,
        new/4,
        execute/2,
        originate/6,
        originate/5,
        get_ami_name/2,
        event_handler_set/3,
        event_handler_del/2
    ]).



new(Username, Secret) ->
    new("localhost", 5038, Username, Secret).

new(Host, Port, Username, Secret) ->
    Client = amitcp:connect(Host, Port),
    amiclient_session:new(Client, Username, Secret).

originate(Interp, Channel, Number, Context, Extension, Priority) ->
    DialedChannel = string:join([Channel, Number], "/"),
    execute(Interp, [
            {action, originate},
            {channel, DialedChannel},
            {context, Context},
            {exten, Extension},
            {'priority', Priority},
            {variable, string:concat("asterisk-ami-name=", Channel)},
            {account, Channel}
        ]).

originate(Interp, Channel, Context, Extension, Priority) ->
    execute(Interp, [
            {action, originate},
            {channel, Channel},
            {context, Context},
            {exten, Extension},
            {'priority', Priority},
            {variable, string:concat("asterisk-ami-name=", Channel)},
            {account, Channel}
        ]).

get_ami_name(Interp, Channel) ->
    execute(Interp, [
            {action, getvar},
            {channel, Channel},
            {variable, "asterisk-ami-name"}
        ]).

event_handler_set(Interp, EventName, {_Module, _Fun, _Args} = Handler) ->
    interp:rpc(Interp, {evtproc_handler_set, EventName, Handler}).

event_handler_del(Interp, EventName) ->
    interp:rpc(Interp, {evtproc_handler_del, EventName}).

execute(_Ami, []) ->
    {error, no_command_specified};
execute(Ami, [{action, _Action} | _Rest] = Command) ->
    interp:rpc(Ami, Command).
