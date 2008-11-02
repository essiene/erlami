-module(amisym_interp).
-export([
        new/0
    ]).

-export([
        init/1,
        handle_event/3,
        handle_sync_event/4,
        handle_info/3,
        terminate/3,
        code_change/4
    ]).

-export([
        insecure/2,
        secure/2
    ]).

-behaviour(gen_fsm).


% public functions

new() ->
    SessionPid = self(),
    case gen_fsm:start_link(?MODULE, SessionPid, []) of
        {ok, Pid} ->
            Pid;
        Any ->
            Any
    end.


% generic gen_fsm callbacks

init(SessionPid) ->
    {ok, insecure, SessionPid}.

handle_event(_Request, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(Request, _From, StateName, State) ->
    {reply, {illegal_request, Request}, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {next_state, StateName, State}.

% State handlers

insecure({SessionPid, change_state}, SessionPid) ->
    {next_state, secure, SessionPid};
insecure({SessionPid, close}, SessionPid) ->
    {stop, normal, SessionPid};
insecure({SessionPid, [{action, "login"} | _Rest] = Command}, SessionPid) ->
    case amisym_actions:a_login(Command, false) of
        {ok, Response} ->
            amisym_session:send_response(SessionPid, Response, Command),
            amisym_eventbus:connect(),
            {next_state, secure, SessionPid};
        {error, Response} ->
            amisym_session:send_response(SessionPid, Response, Command),
            {next_state, insecure, SessionPid}
    end;
insecure({SessionPid, Command}, SessionPid) ->
    amisym_session:send_response(SessionPid, amisym_actions:a_not_logged_in(Command, false), Command),
    {next_state, insecure, SessionPid};
insecure(_Request, State) ->
    {next_state, insecure, State}.


secure({SessionPid, close}, SessionPid) ->
    amisym_eventbus:disconnect(),
    {stop, normal, SessionPid};
secure({SessionPid, [{action, "logout"} | _Rest] = Command}, SessionPid) ->
    amisym_session:send_response(SessionPid, amisym_actions:a_logout(Command, true), Command),
    {next_state, insecure, SessionPid};
secure({SessionPid, [{action, Action} | _Rest] = Command}, SessionPid) ->
    Response = amisym_actions:action(Action, Command, true),
    amisym_session:send_response(SessionPid, Response, Command),
    {next_state, secure, SessionPid};
secure({amisym_eventbus, [{event, _EventName} | _Rest] = Event}, SessionPid) ->
    amisym_session:send_event(SessionPid, Event),
    {next_state, secure, SessionPid};
secure(_Request, State) ->
    {next_state, secure, State}.
