-module(amisym_interp).
-export([
        start/1,
        state_not_logged_in/1,
        state_logged_in/1,
        send/3
    ]).
-include("ami.hrl").


send(InterpPid, SessionPid, AmiList) ->
    InterpPid ! {SessionPid, AmiList}.


start(SessionPid) ->
    spawn_link(?MODULE, state_not_logged_in, [SessionPid]).

state_not_logged_in(SessionPid) ->
    receive
        {SessionPid, {cmd, exit}} ->
            do_nothing;
        {SessionPid, [{action, "login"} | _Rest] = Command} ->
            case amisym_actions:a_login(Command, false) of
                {ok, Response} ->
                    send_response(Command, Response, SessionPid),
                    state_logged_in(SessionPid);
                {error, Response} ->
                    send_response(Command, Response, SessionPid),
                    state_not_logged_in(SessionPid)
            end;
        {SessionPid, Command} ->
            send_response(Command, amisym_actions:a_not_logged_in(Command, false), SessionPid),
            state_not_logged_in(SessionPid)
    end.

state_logged_in(SessionPid) ->
    receive 
        {SessionPid, {cmd, exit}} ->
            do_nothing;
        {SessionPid, [{action, "logout"} | _Rest] = Command} ->
            send_response(Command, amisym_actions:a_logout(Command, true), SessionPid),
            state_not_logged_in(SessionPid);
        {SessionPid, [{action, Action} | _Rest] = Command} ->
            send_response(Command, apply(amisym_actions, list_to_atom(?SYM_ACTION_FUNCTION_PREFIX ++ Action), [Command, true]), SessionPid),
            state_logged_in(SessionPid)
    end.

send_response(Command, Response, SessionPid) ->
    case amilist:get_value(Command, actionid) of
        {keyerror, _Key} ->
            SessionPid ! {self(), Response};
        ActionId ->
            Response1 = amilist:set_value(Response, actionid, ActionId),
            SessionPid ! {self(), Response1}
    end.
