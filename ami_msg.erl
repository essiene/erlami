-module(ami_msg).
-export([
        start/1,
        loop/3,
        ping/0,
        send/1,
        notify/1,
        stop/0,
        stop/1
    ]).


start(Socket) ->
    register(ami_msg, spawn(?MODULE, loop, [Socket, 0, dict:new()])).

ping() ->
    ami_msg ! {ping, self()},
    receive
        Any ->
            Any 
    after 5000 ->
            {error, no_response}
    end.

notify(Message) ->
    ami_msg ! Message.

send(Command) ->
    ami_msg ! {cmd, self(), Command},
    receive
        Any ->
            Any
    after 5000 ->
            {error, no_response}
    end.

stop(Reason) ->
    ami_msg ! {stop, self(), Reason},
    receive 
        Any ->
            Any
    after 5000 ->
            {error, no_response}
    end.

stop() ->
    ami_msg ! {stop, self(), "Default Reason"},
    receive 
        Any ->
            Any
    after 5000 ->
            {error, no_response}
    end.



loop(Socket, Tid, Dict) ->
    receive
        {stop, Pid, Reason} ->
            gen_tcp:close(Socket),
            Pid ! {ok, stopped, Reason};
        {ping, Pid} ->
            Pid ! {ok, pong},
            loop(Socket, Tid, Dict);
        {cmd, Pid, Command} ->
            NewCommand = [{actionid, integer_to_list(Tid)} | Command],
            Data = ami_util:build_command(NewCommand),
            case gen_tcp:send(Socket, Data) of
                {error, Reason} ->
                    Pid ! {error, Reason},
                    loop(Socket, Tid, Dict);
                ok ->
                    NewDict = dict:store(Tid, Pid, Dict),
                    Pid ! {ok, Tid},
                    loop(Socket, Tid + 1, NewDict)
            end;
        {response, Response, RDict} ->
            ami_util:logmessage(dict:to_list(RDict)),
            ActionId = list_to_integer(dict:fetch(actionid, RDict)),
            RDict1 = dict:erase(actionid, RDict),
            RDict2 = dict:erase(response, RDict1),
            case dict:find(ActionId, Dict) of
                {ok, RPid} -> 
                    RPid ! {Response, RDict2}, 
                    loop(Socket, Tid, Dict);
                error ->
                    loop(Socket, Tid, Dict)
            end;
        {event, _, Edict} ->
            ami_util:logmessage(dict:to_list(Edict)),
            loop(Socket, Tid, Dict)
    end.


