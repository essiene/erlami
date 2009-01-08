% AMI socket is a special tcp socket that makes it easy to handle
% connected or disconnected ami states and can easily reconnect
% when the endpoint comes back online.
%
% States are:
%   disconnected: Open but not connected to end point
%               Recv gives an error (not closed), Send gives
%               an error, No asnc notifications are recv'd
%
%   connected : Open and connected to end point
%               Recv works, Send works, async notifications
%               recv'd on active|once|true recieves.
%
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
        connect/3,
        connect/4,
        getopts/2
%        connect/4,
%        send/2,
%        recv/2,
%        recv/3,
%        controlling_process/2,
%        close/1,
%        shutdown/2
    ]).

-export([
        disconnected/2,
        disconnected/3,
        connected/2,
        connected/3,
        closed/2,
        closed/3
    ]).

-export([
        init/1,
        handle_sync_event/4,
        handle_event/3,
        handle_info/3,
        terminate/3,
        code_change/4
    ]).

-include("ami.hrl").

connect(Address, Port, Opts) ->
    connect(Address, Port, Opts, infinity).

connect(Address, Port, Opts0, Timeout) ->
    {Opts1, WaitRetry} = util:proplists_remove(Opts0, ami_retry, ?AMI_SOCKET_RETRY),

    case gen_fsm:start(?MODULE, [WaitRetry, Address, Port, Opts1, Timeout], []) of
        {ok, Pid} ->
            {ok, #ami_socket{pid=Pid}};
        {error, Reason} ->
            {error, Reason}
    end.

getopts(Sock, Opts) when is_record(Sock, ami_socket) ->
    gen_fsm:sync_send_all_state_event(Sock#ami_socket.pid, {getopts, Opts});
getopts(St, Opts) ->
    getopts(St, Opts, []).


% gen_fsm states

disconnected(Event, _From, St) ->
    {reply, {illegal_event, Event}, disconnected, St}.


disconnected({connect, Timeout}, #ami_socket_state{host=Host, port=Port, opts=Opts}=St) ->
    case gen_tcp:connect(Host, Port, Opts, Timeout) of
        {ok, Sock} ->
            {next_state, connected, St#ami_socket_state{sock=Sock}};
        {error, Reason} ->
            error_logger:error_report([ami_socket, {state, disconnected}, {error, Reason}]),
            gen_fsm:send_event_after(St#ami_socket_state.wait_retry, {connect, Timeout}),
            {next_state, disconnected, St}
    end;

disconnected(_Event, St) ->
    {next_state, disconnected, St}.





connected(Event, _From, St) ->
    {reply, {illegal_event, Event}, disconnected, St}.

connected(_Event, St) ->
    {next_state, connected, St}.




closed(_Event, _From, St) ->
    {stop, normal, {error, closed}, St}.

closed(_Event, St) ->
    {stop, normal, St}.

% gen_fsm callbacks

init([WaitRetry, Address, Port, Options, Timeout]) ->
    St = #ami_socket_state{
        host=Address,
        port=Port,
        opts=Options,
        wait_retry=WaitRetry
    },

    gen_fsm:send_event_after(500, {connect, Timeout}),
    {ok, disconnected, St}.

handle_sync_event({getopts, Opts}, _From, StateName, St) ->
    {reply, getopts(St, Opts), StateName, St};

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



getopts(_St, [], Accm) ->
    {ok, lists:reverse(Accm)};
getopts(St, [ami_host | Rest], Accm) ->
    getopts(St, Rest, [{ami_host, St#ami_socket_state.host} | Accm]);
getopts(St, [ami_port | Rest], Accm) ->
    getopts(St, Rest, [{ami_port , St#ami_socket_state.port} | Accm]);
getopts(St, [ami_retry | Rest], Accm) ->
    getopts(St, Rest, [{ami_retry, St#ami_socket_state.wait_retry} | Accm]);
getopts(#ami_socket_state{sock=undefined}=St, [Other | Rest], Accm) ->
    case proplists:lookup(Other, St#ami_socket_state.opts) of
        none -> 
            {error, einval};
        {Other, Value} ->
            getopts(St, Rest, [{Other, Value} | Accm])
    end;
getopts(St, [Other | Rest], Accm) ->
    case inet:getopts(St#ami_socket_state.sock, [Other]) of
        {ok, [{Other, Value}]} ->
            getopts(St, Rest, [{Other, Value} | Accm]); 
        {error, Reason} ->
            {error, Reason}
    end.
