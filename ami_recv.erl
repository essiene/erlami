-module(ami_recv).
-export([
        start/0,
        loop/1,
        send_list_of_string/1
    ]).

start() ->
    spawn(?MODULE, loop, [""]).


loop(Remainder) ->
    receive
        {ping, Pid} ->
            Pid ! {ok, pong},
            loop(Remainder);
        {stop, Pid, Reason} ->
            Pid ! {stop, Reason};
        {tcp, _Socket, Data} ->
            NewData = string:concat(Remainder, Data),
            {ListOfStrings, NewRemainder} = processor:extract_packets(NewData, "\r\n\r\n"),
            async_send_list_of_string(ListOfStrings),
            loop(NewRemainder);
        {tcp_closed, Socket} ->
            gen_tcp:close(Socket);
        {tcp_error, _, Reason} ->
            logmessage(Reason),
            loop(Remainder)
    end.

async_send_list_of_string(ListOfStrings) ->
    spawn(?MODULE, send_list_of_string, [ListOfStrings]).

send_list_of_string([]) -> 
    true;
send_list_of_string([H | T]) ->
    Dict = ami_util:parse_response(H),
    case dict:find(response, Dict) of
        {ok, Response} ->
            ami_msg:notify({response, Response, Dict});
        error ->
            case dict:find(event, Dict) of
                {ok, Event} ->
                    ami_msg:notify({event, Event, Dict});
                error ->
                    logmessage({invalid_message, dict:to_list(Dict)})
            end
    end,
    send_list_of_string(T).


logmessage(Message) ->
    io:format("~s~n", [Message]).
