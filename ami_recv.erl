-module(ami_recv).
-export([
        start/1,
        loop/2,
        send_list_of_string/2
    ]).

start(CmdPid) ->
    spawn(?MODULE, loop, [CmdPid, ""]).


loop(CmdPid, Remainder) ->
    logmessage("~n==========Rcv Loop=========~n"),
    receive
        {tcp, _Socket, Data} ->
            NewData = string:concat(Remainder, Data),
            {ListOfStrings, NewRemainder} = processor:extract_packets(NewData, "\r\n\r\n"),
            async_send_list_of_string(ListOfStrings, CmdPid),
            loop(CmdPid, NewRemainder);
        {tcp_closed, Socket} ->
            gen_tcp:close(Socket),
            true;
        {tcp_error, _, Reason} ->
            logmessage(Reason),
            loop(CmdPid, Remainder)
    end.

async_send_list_of_string(ListOfStrings, CmdPid) ->
    spawn(?MODULE, send_list_of_string, [ListOfStrings, CmdPid]).

send_list_of_string([], _) -> 
    true;
send_list_of_string([H | T], CmdPid) ->
    Dict = ami_util:parse_response(H),
    case dict:find(response, Dict) of
        {ok, Response} ->
            CmdPid ! {response, Response, Dict};
        error ->
            case dict:find(event, Dict) of
                {ok, Event} ->
                    CmdPid ! {event, Event, Dict};
                error ->
                    true
            end
    end,
    send_list_of_string(T, CmdPid).


logmessage(Message) ->
    io:format("~s~n", [Message]).
