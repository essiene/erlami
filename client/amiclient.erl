-module(amiclient).
-export([
        start/0,
        loop/1,
        connect/2,
        banner/1,
        send_list_of_string/1
    ]).

-include("ami.hrl").

start() ->
    spawn(?MODULE, loop, [""]).

%% ---------------------------------------------------------------
%% @doc
%% @spec ami:init(Host, Port, Username, Secret) -> 
%%  {ok, Socket} | throw({ami_connect, Reason})
%% @end
%% ---------------------------------------------------------------

connect(Host, Port) ->
    case gen_tcp:connect(Host, Port, ?CONNECT_OPTION) of
        {ok, Socket} -> 
            Socket;
        {error, Reason} ->
            throw({connect, Reason})
    end.

%%---------------------------------------------------------
%% @doc
%% @spec banner(Socket) -> {ok, Banner} | {error, Reason}.
%% @end
%%----------------------------------------------------------

banner(Socket) ->
    case gen_tcp:recv(Socket, 0) of 
        {ok, Banner} ->
            B1 = util:strip(Banner),
            case B1 =:= "Asterisk Call Manager/1.0" of
                true ->
                    {ok, B1};
                false ->
                    throw({banner, B1})
            end;
        {error, Reason} ->
            throw({banner, Reason})
    end.

%%-----------------------------------------------------------
%% @doc
%% @spec login(Socket, Username, Secret) ->
%%      {ok, Socket} | {error, Reason}
%% @end
%%-----------------------------------------------------------



login(Socket, Username, Secret) ->
    TupleList = [{action, "login"}, {username, Username}, {secret, Secret}],
    Block = amilist:to_block(TupleList),
    case get_tcp:send(Socket, Block) of
        ok ->
            {ok, sent};
        {error, Reason} ->
            {error, Reason}
    end.

send(Socket, Action, Params, ActionId) ->
    Id = integer_to_list(ActionId),
    Command = ami_util:build_command([{action, Action} , {actionid, Id} |  Params]),
    case gen_tcp:send(Socket, Command) of
        ok -> 
            {ok, ActionId};
        {error, Reason} ->
            {error, Reason}
    end.

first_recv(Socket, Remainder) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} -> 
            NewData = string:concat(Remainder, Data),
            case processor:extract_packets(NewData, "\r\n\r\n") of
                {[CompleteData], ""} ->
                    ResponseDict = ami_util:parse_response(CompleteData),
                    Message = dict:fetch(message, ResponseDict),
                    case dict:find(response, ResponseDict) of
                        {ok, "Success"} ->
                            {ok, Message};
                        {ok, "Error"} ->
                            {error, Message}
                    end;
                {_, NewRemainder} ->
                    first_recv(Socket, NewRemainder)
            end;
        {error, Reason} ->
            {error, Reason}
    end.


%loop(Remainder) ->
%    receive
%        {ping, Pid} ->
%            Pid ! {ok, pong},
%            loop(Remainder);
%        {stop, Pid, Reason} ->
%            Pid ! {stop, Reason};
%        {tcp, _Socket, Data} ->
%            NewData = string:concat(Remainder, Data),
%            {ListOfStrings, NewRemainder} = processor:extract_packets(NewData, "\r\n\r\n"),
%            async_send_list_of_string(ListOfStrings),
%            loop(NewRemainder);
%        {tcp_closed, Socket} ->
%            gen_tcp:close(Socket);
%        {tcp_error, _, Reason} ->
%            ami_util:logmessage(Reason),
%            loop(Remainder)
%    end.

%async_send_list_of_string(ListOfStrings) ->
%    spawn(?MODULE, send_list_of_string, [ListOfStrings]).

%send_list_of_string([]) -> 
%    true;
%send_list_of_string([H | T]) ->
%    Dict = ami_util:parse_response(H),
%    case dict:find(response, Dict) of
%        {ok, Response} ->
%            ami_msg:notify({response, Response, Dict});
%        error ->
%            case dict:find(event, Dict) of
%                {ok, Event} ->
%                    ami_msg:notify({event, Event, Dict});
%                error ->
%                    ami_util:logmessage("****Invalid Message****"),
%                    ami_util:logmessage(dict:to_list(Dict)),
%                    ami_util:logmessage("***********************")
%            end
%    end,
%    send_list_of_string(T).
