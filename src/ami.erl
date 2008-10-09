-module(ami).
-author("Essien Ita Essien <essien.ita@uknglobal.com>").
-export([
        new/2,
        new/4,
        init/2,
        init/4,
        send_cmd/1,
        originate/4
    ]).

-define(CONNECT_OPTION, [list, inet, {active, false}]).

new(Host, Port, Username, Secret) ->
    register(ami_recv, spawn(?MODULE, init, [Host, Port, Username, Secret])).
new(Username, Secret) ->
    register(ami_recv, spawn(?MODULE, init, [Username, Secret])).

init(Username, Secret) ->
    init("localhost", 5038, Username, Secret).

init(Host, Port, Username, Secret) ->
    case connect(Host, Port, Username, Secret) of
        {error, Reason} ->
            {error, Reason};
        {ok, Socket} ->
            ami_msg:start(Socket),
            inet:setopts(Socket, [{active, true}]),
            ami_recv:loop("")
    end.


originate(Channel, Context, Extension, Priority) ->
    send_cmd([
            {action, "originate"},
            {channel, Channel},
            {context, Context},
            {exten, Extension},
            {pRiority, Priority}
        ]).

send_cmd(Command) ->
    case ami_msg:send(Command) of
        {ok, _} -> 
            get_response();
        Other ->
            Other
    end.


get_response() ->
    receive
        Message ->
            Message
    end.

%%-----------------------
%% ami:init(AsteriskHost, AMIPort, Username, Secret) ->
%%  {ok, AMI} | {error, Reason}
%%------------------------

connect(AsteriskHost, AMIPort, Username, Secret) ->
    case gen_tcp:connect(AsteriskHost, AMIPort, ?CONNECT_OPTION) of
        {ok, Socket} -> 
            case banner(Socket) of
                {ok, Banner} ->
                    ami_util:logmessage(Banner),
                    case login(Socket, Username, Secret) of
                        {error, Reason} ->
                            ami_util:logmessage(Reason),
                            {error, Reason};
                        {ok, Message} ->
                            ami_util:logmessage(Message),
                            {ok, Socket}
                    end;
                {error, Reason} ->
                    ami_util:logmessage(Reason),
                    {error, Reason}
            end;
        {error, Reason} ->
            ami_util:logmessage(Reason),
            {error, Reason}
    end.




%%=======================================================
%% Private Functions
%%========================================================

%%---------------------------------------------------------
%% @doc
%% @spec banner(Socket) -> {ok, Banner} | {error, Reason}.
%% @end
%%----------------------------------------------------------

banner(Socket) ->
    case gen_tcp:recv(Socket, 0) of 
        {ok, Banner} ->
            Banner1 = string:strip(Banner),
            Banner2 = string:strip(Banner1),
            {ok, Banner2};
        {error, Reason} ->
            {error, Reason}
    end.

%%-----------------------------------------------------------
%% @doc
%% @spec login(Socket, Username, Secret) ->
%%      {ok, Socket} | {error, Reason}
%% @end
%%-----------------------------------------------------------



login(Socket, Username, Secret) ->
    case send(Socket, "login", [{username, Username}, {secret, Secret}], 0) of
        {ok, _} ->
            first_recv(Socket, "");
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
