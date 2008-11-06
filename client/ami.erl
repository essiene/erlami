-module(ami).
-author("Essien Ita Essien <essien.ita@uknglobal.com>").
-export([
        new/2,
        new/4,
        close/1,
        execute/2,
        originate/5,
        originate/6,
        originate/7,
        get_ami_name/2,
        event_handler_add/3,
        event_handler_del/2,
        event_handler_get/1
    ]).



new(Username, Secret) ->
    new("localhost", 5038, Username, Secret).

new(Host, Port, Username, Secret) ->
    Client = amitcp:connect(Host, Port),
    amiclient_session:new(Client, Username, Secret).

originate(Ami, Channel, Context, Extension, Priority) ->
    originate(Ami, Channel, Context, Extension, Priority, 30000, []).

originate(Ami, Channel, Number, Context, Extension, Priority) ->
    DialedChannel = string:join([Channel, Number], "/"),
    originate(Ami, DialedChannel, Context, Extension, Priority).


originate(Ami, Channel, Context, Extension, Priority, Timeout, ChanVars) ->
    ChanVarList = util:build_chan_vars(ChanVars),
    execute(Ami, [
            {action, originate},
            {channel, Channel},
            {context, Context},
            {exten, Extension},
            {'priority', Priority},
            {account, Channel},
            {timeout, Timeout},
            {variable, string:concat("asterisk-ami-name=", Channel)} | ChanVarList
        ], Timeout + 2000).

get_ami_name(Ami, Channel) ->
    Response = execute(Ami, [
            {action, getvar},
            {channel, Channel},
            {variable, "asterisk-ami-name"}
        ]),
    amilist:get_value(Response, value).

event_handler_add({_Session, Interp}, Handler, Args) ->
    interp:rpc(Interp, {handler_add, Handler, Args}).

event_handler_del({_Session, Interp}, Handler) ->
    interp:rpc(Interp, {handler_del, Handler}).

event_handler_get({_Session, Interp}) ->
    interp:rpc(Interp, handler_get).

close({_Session, Interp}=_Ami) ->
    interp:close(Interp).

execute({_Session, _Interp}, []) ->
    {error, no_command_specified};
execute({_Session, Interp}, [{action, _Action} | _Rest] = Command) ->
    interp:rpc(Interp, Command).

execute({_Session, _Interp}, [], _Timeout) ->
    {error, no_command_specified};
execute({_Session, Interp}, [{action, _Action} | _Rest] = Command, Timeout) ->
    interp:rpc(Interp, Command, Timeout).
