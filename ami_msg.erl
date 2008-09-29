-module(ami_msg).
-export([
        start/1,
        loop/3,
        ping/0,
        send/1,
        notify/1,
        stop/0
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

stop() ->
    ami_msg ! {stop, self()},
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
            logmessage(dict:to_list(RDict)),
            Message = dict:fetch(message, RDict),
            ActionId = list_to_integer(dict:fetch(actionid, RDict)),
            case dict:find(ActionId, Dict) of
                {ok, RPid} -> 
                    RPid ! {response, Response, Message}, 
                    loop(Socket, Tid, Dict);
                error ->
                    loop(Socket, Tid, Dict)
            end;
        {event, _, Edict} ->
            logmessage(dict:to_list(Edict)),
            loop(Socket, Tid, Dict)
    end.

logmessage([]) ->
    io:format("~n");
logmessage([{Key, Value} | T]) when is_list(Value) ->
    io:format("~w~n", [{Key, list_to_atom(Value)}]),
    logmessage(T);
logmessage(Message) when is_list(Message)->
    io:format("~s~n", [Message]);
logmessage(Any) ->
    io:format("~w~n", [Any]).
