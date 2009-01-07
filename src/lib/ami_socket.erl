% AMI socket is a special tcp socket that makes it easy to handle
% connected or disconnected ami states and can easily reconnect
% when the endpoint comes back online.
%
% States are:
%   connected : Open and connected to end point
%               Recv works, Send works, async notifications
%               recv'd on active|once|true recieves.

%   disconnected: Open but not connected to end point
%               Recv gives an error (not closed), Send gives
%               an error, No asnc notifications are recv'd
%
%   closed    : Closed and not able to communicate.
% 
% This socket mimics the semantics of a normal gen_tcp socket
% connection and can operate in active -> true | once| false modes
% also. The only difference is the Signals delivered when active 
% is true or once are:
%    {ami, Socket, Data}, 
%    {ami_close, Socket}, 
%    {ami_error, Socket, Reason}
%
-module(ami_socket).
-behaviour(gen_fsm).

-export([
        connect/3
%        connect/4,
%        send/2,
%        recv/2,
%        recv/3,
%        controlling_process/2,
%        close/1,
%        shutdown/2
    ]).

-export([
        init/1,
        handle_sync_event/4,
        handle_event/3,
        handle_info/3,
        terminate/3,
        code_change/4
    ]).

-export([
        amiopts_get/2, 
        amiopts_get/3
    ]).

-include("ami.hrl").

connect(Address, Port, Opts0) ->
    {Opts1, Username} = amiopts_get(Opts0, ami_username),
    {Opts2, Secret} = amiopts_get(Opts1, ami_secret),
    {Opts3, WaitRetry} = amiopts_get(Opts2, ami_retry, ?AMI_SOCKET_RETRY),

    gen_fsm:start(?MODULE, [Username, Secret, WaitRetry, Address, Port, Opts3], []).



% gen_fsm callbacks

init([Username, Secret, WaitRetry, Address, Port, Options]) ->
    St = #ami_socket_state{
        username=Username,
        secret=Secret,
        host=Address,
        port=Port,
        opts=Options,
        wait_retry=WaitRetry
    },

    gen_fsm:send_event_after(500, connect),
    {ok, disconnected, St}.

handle_sync_event(Event, _From, StateName, St) ->
    {reply, {illegal_event, Event}, StateName, St}.

handle_event(_Event, StateName, St) ->
    {next_state, StateName, St}.

handle_info(_Info, StateName, St) ->
    {next_state, StateName, St}.

terminate(_Reason, _StateName, _St) ->
    ok.

code_change(_OldVsn, StateName, St, _Extra) ->
    {next_state, StateName, St}.


amiopts_get(List0, Key, Default) ->
    try
        amiopts_get(List0, Key)
    catch
        throw:{not_found, Key} ->
            List1 = proplists:delete(Key, List0),
            {List1, Default}
    end.


amiopts_get(List0, Key) ->
    case proplists:lookup(Key, List0) of
        none ->
            throw({not_found, Key});
        {Key, Val} ->
            List1 = proplists:delete(Key, List0),
            {List1, Val}
    end.
