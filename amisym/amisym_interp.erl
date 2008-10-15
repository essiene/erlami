-module(amisym_interp).
-export([
        start/1,
        state_not_logged_in/1,
        state_logged_in/1,
        send/3
    ]).


send(InterpPid, SessionPid, AmiList) ->
    InterpPid ! {SessionPid, AmiList}.


start(SessionPid) ->
    spawn_link(?MODULE, state_not_logged_in, [SessionPid]).

state_not_logged_in(SessionPid) ->
    receive
        {SessionPid, [{action, "login"} | _Rest] = Command} ->
            case login(Command) of
                {ok, Response} ->
                    SessionPid ! {self(), Response},
                    state_logged_in(SessionPid);
                {error, Response} ->
                    SessionPid ! {self(), Response},
                    state_not_logged_in(SessionPid)
            end;
        _Other ->
            SessionPid ! {self(), [{response, "Error"}, {message, "Not Logged In"}]},
            state_not_logged_in(SessionPid)
    end.

state_logged_in(SessionPid) ->
    receive 
        {SessionPid, [{action, "login"} | _Rest]} ->
            SessionPid ! {self(), [{response, "Success"}, {message, "Already Logged In"}]},
            state_logged_in(SessionPid);
        {SessionPid, [{action, "command"}, {'command', "core show version"} | _Rest] = Command} ->
            SessionPid ! {self(), command_core_show_version(Command)},
            state_logged_in(SessionPid)
    end.


login(Command) ->
    Username = amilist:get_value(Command, username),
    Secret = amilist:get_value(Command, secret),
    if
        Username =:= "sym" ->
            if 
                Secret =:= "sym" ->
                    {ok, [{response, "Success"}, {message, "Authentication Successfull"}]}; 
                true -> 
                    {error, [{response, "Error"}, {message, "Authentication Failed"}]} 
            end;
        true ->
            {error, [{response, "Error"}, {message, "Authentication Failed"}]}
    end.

command_core_show_version(Command) ->
    ActionId = amilist:get_value(Command, actionid),
    [{response, "Success"}, {message, "AMISym 0.1"}, {actionid, ActionId}].
