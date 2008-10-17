-module(amisym_interp).
-export([
        new/1,
        state_not_logged_in/1,
        state_logged_in/1,
        interpret_blocks/3,
        interpret_blocks/4
    ]).




new(SessionPid) ->
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
                    send_response(Command, Response, SessionPid),
                    state_logged_in(SessionPid);
                {error, Response} ->
                    send_response(Command, Response, SessionPid),
                    state_not_logged_in(SessionPid)
            end;
        {SessionPid, Command} ->
            send_response(Command, amisym_actions:a_not_logged_in(Command, false), SessionPid),
            state_not_logged_in(SessionPid);
        _Any ->
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
            Response = amisym_actions:action(Action, Command, true),
            send_response(Command, Response, SessionPid),
            state_logged_in(SessionPid);
        _Any ->
            state_logged_in(SessionPid)
    end.

send_response(Command, Response, SessionPid) ->
    case amilist:get_value(Command, actionid) of
        {error, {no_key, _Key}} ->
            SessionPid ! {self(), Response};
        ActionId ->
            Response1 = amilist:set_value(Response, actionid, ActionId),
            SessionPid ! {self(), Response1}
    end.


interpret_blocks(Interp, ListOfBlocks, SessionPid) ->
    spawn(?MODULE, interpret_blocks, [Interp, ListOfBlocks, SessionPid, dummy]).

interpret_blocks(_Interp, [], _SessionPid, dummy) -> 
    true;
interpret_blocks(Interp, [Block | Rest], SessionPid, dummy) ->
    AmiList = messaging:block_to_amilist(Block),
    case amilist:has_key(AmiList, action) of
        true ->
            interpret_amilist(Interp, AmiList, SessionPid);
        false ->
            util:logmessage("****Invalid Message****"),
            util:logmessage(AmiList),
            util:logmessage("***********************")
    end,
    interpret_blocks(Interp, Rest, SessionPid, dummy).


interpret_amilist(Interp, AmiList, SessionPid) ->
    Interp ! {SessionPid, AmiList}.
