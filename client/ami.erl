-module(ami).
-author("Essien Ita Essien <essien.ita@uknglobal.com>").
-export([
        new/2,
        new/4,
        close/1,
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


originate(Ami, Channel, Number, Context, Extension, Priority) ->
    DialedChannel = string:join([Channel, Number], "/"),
    execute(Ami, [
            {action, originate},
            {channel, DialedChannel},
            {context, Context},
            {exten, Extension},
            {'priority', Priority},
            {account, Channel},
            {variable, string:concat("asterisk-ami-name=", Channel)}
        ]).

originate(Ami, Channel, Context, Extension, Priority) ->
    execute(Ami, [
            {action, originate},
            {channel, Channel},
            {context, Context},
            {exten, Extension},
            {'priority', Priority},
            {account, Channel},
            {variable, string:concat("asterisk-ami-name=", Channel)}
        ]).

get_ami_name(Ami, Channel) ->
    Response = execute(Ami, [
            {action, getvar},
            {channel, Channel},
            {variable, "asterisk-ami-name"}
        ]),
    amilist:get_value(Response, value).

event_handler_set({_Session, Interp}, EventName, {_Module, _Fun, _Args} = Handler) ->
    interp:rpc(Interp, {evtproc_handler_set, EventName, Handler}).

event_handler_del({_Session, Interp}, EventName) ->
    interp:rpc(Interp, {evtproc_handler_del, EventName}).

close({_Session, Interp}=_Ami) ->
    interp:close(Interp).

execute({_Session, _Interp}, []) ->
    {error, no_command_specified};
execute({_Session, Interp}, [{action, _Action} | _Rest] = Command) ->
    interp:rpc(Interp, Command).
