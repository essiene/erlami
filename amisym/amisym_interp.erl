-module(amisym_interp).
-export([
        new/0,
        state_not_logged_in/1,
        state_logged_in/1
    ]).


new() ->
    SessionPid = self(),
    spawn_link(?MODULE, state_not_logged_in, [SessionPid]).

state_not_logged_in(SessionPid) ->
    receive
        {SessionPid, {cmd, change_state}} ->
            state_logged_in(SessionPid);
        {SessionPid, {cmd, exit}} ->
            do_nothing;
        {SessionPid, [{action, "login"} | _Rest] = Command} ->
            case amisym_actions:a_login(Command, false) of
                {ok, Response} ->
                    amisym_session:send_response(SessionPid, Response, Command),
                    amisym_eventbus:connect(),
                    state_logged_in(SessionPid);
                {error, Response} ->
                    amisym_session:send_response(SessionPid, Response, Command),
                    state_not_logged_in(SessionPid)
            end;
        {SessionPid, Command} ->
            amisym_session:send_response(SessionPid, amisym_actions:a_not_logged_in(Command, false), Command),
            state_not_logged_in(SessionPid);
        _Any ->
            state_not_logged_in(SessionPid)
    end.

state_logged_in(SessionPid) ->
    receive 
        {SessionPid, {cmd, exit}} ->
            amisym_eventbus:disconnect(),
            do_nothing;
        {SessionPid, [{action, "logout"} | _Rest] = Command} ->
            amisym_session:send_response(SessionPid, amisym_actions:a_logout(Command, true), Command),
            state_not_logged_in(SessionPid);
        {SessionPid, [{action, Action} | _Rest] = Command} ->
            Response = amisym_actions:action(Action, Command, true),
            amisym_session:send_response(SessionPid, Response, Command),
            state_logged_in(SessionPid);
        {amisym_eventbus, [{event, _EventName} | _Rest] = Event} ->
            amisym_session:send_event(SessionPid, Event),
            state_logged_in(SessionPid);
        _Any ->
            state_logged_in(SessionPid)
    end.
