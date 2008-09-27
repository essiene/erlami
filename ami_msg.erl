-module(ami_msg).
-export([
        start/1,
        loop/3
    ]).


start(Socket) ->
    spawn(?MODULE, loop, [Socket, 0, dict:new()]).


loop(Socket, Tid, Dict) ->
    receive
        {cmd, Pid, Command} ->
            NewCommand = [{actionid, integer_to_list(Tid)} | Command],
            Data = ami_util:build_command(NewCommand),
            case gen_tcp:send(Socket, Data) of
                {error, Reason} ->
                    Pid ! {error, Reason};
                ok ->
                    NewDict = dict:store(Tid, Pid, Dict),
                    loop(Socket, Tid + 1, NewDict)
            end;
        {response, Response, RDict} ->
            logmessage(string:concat("Response => ", Response)),
            Message = dict:fetch(message, RDict),
            ActionId = list_to_integer(dict:fetch(actionid, RDict)),
            RPid = dict:fetch(ActionId, Dict),
            RPid ! {response, Response, Message},
            loop(Socket, Tid, Dict);
        {event, Event, _} ->
            logmessage(Event),
            loop(Socket, Tid, Dict)
    end.

logmessage(Message) ->
    io:format("~s~n", [Message]).
