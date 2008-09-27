-module(ami).
-author("Essien Ita Essien <essien.ita@uknglobal.com>").
-export([
        new/2,
        new/4,
        originate/7
    ]).

-define(CONNECT_OPTION, [list, inet, {active, false}, {recbuf, 1024000}]).


%%-----------------------
%% ami:new(AsteriskHost, AMIPort, Username, Secret) ->
%%  {ok, AMI} | {error, Reason}
%%------------------------

new(Username, Secret) ->
    new("localhost", 5038, Username, Secret).

new(AsteriskHost, AMIPort, Username, Secret) ->
    case gen_tcp:connect(AsteriskHost, AMIPort, ?CONNECT_OPTION) of
        {ok, Socket} -> 
            case banner(Socket) of
                {ok, Banner} -> 
                    logmessage(Banner),
                    case login(Socket, Username, Secret) of
                        {ok, Response} -> 
                            logmessage(Response),
                            CmdPid = ami_msg:start(Socket),
                            RcvPid = ami_recv:start(CmdPid),
                            inet:setopts(Socket, [{action, true}]),
                            gen_tcp:controlling_process(Socket, RcvPid),
                            CmdPid;
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

logmessage(Message) ->
    io:format("~s~n", [Message]).



originate(AMI, Chan, Ctx, Exten, Prio, Timeout, Vars) ->
    case send(AMI, "originate", [
                {channel, Chan},
                {context, Ctx},
                {exten, Exten},
                {pRiority, Prio}, % priority is a keyword
                {timeout, Timeout}
                | ami_util:build_vars(Vars)]) of
        {ok, Response} ->
            {ok, Response};
        {error, Reason} ->
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
            {ok, string:strip(Banner)};
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
    case send(Socket, "login", [{username, Username}, {secret, Secret}]) of
        {ok, Response} ->
            {ok, Response};
        {error, Reason} ->
            {error, Reason}
    end.

send(Socket, Action, Params) ->
    Command = ami_util:build_command([{action, Action} , {actionid, "11223"} |  Params]),
    case gen_tcp:send(Socket, Command) of
        ok -> 
            recv(Socket);
        {error, Reason} ->
            {error, Reason}
    end.

recv(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Packet} -> 
            ResponseDict = ami_util:parse_response(Packet), 
            check_response(ResponseDict);
        {error, Reason} ->
            {error, Reason}
    end.


check_response(Dict) ->
    case dict:find(response, Dict) of
        {ok, Response} ->
            case dict:find(actionid, Dict) of
                {ok, _} ->
                    case dict:find(message, Dict) of
                        {ok, Message} ->
                            logmessage("Found Message"),
                            case Response of
                                "Success" ->
                                    {ok, Message};
                                "Error" ->
                                    {error, Message}
                            end;
                        error ->
                            logmessage("Message missing")
                    end;
                error ->
                    logmessage("ActionId missing")
            end;
        error -> 
            io:format("Invalid Response => ~w~n", [Dict])
    end.
