-module(amisym_interp).
-export([
        start/1,
        loop/1,
        send/3
    ]).


send(InterpPid, SessionPid, AmiList) ->
    InterpPid ! {SessionPid, AmiList}.


start(SessionPid) ->
    spawn_link(?MODULE, loop, [SessionPid]).

loop(SessionPid) ->
    receive 
        {SessionPid, [{action, "login"} | _Rest] = Command} ->
            SessionPid ! {self(), do_login(Command)},
            loop(SessionPid);
        {SessionPid, [{action, "command"}, {'command', "core show version"} | _Rest] = Command} ->
            SessionPid ! {self(), command_core_show_version(Command)},
            loop(SessionPid)
    end.


do_login(_Command) ->
    [{response, "Success"}, {message, "Authentication Successfull"}].

command_core_show_version(Command) ->
    ActionId = amilist:get_value(Command, actionid),
    [{response, "Success"}, {message, "AMISym 0.1"}, {actionid, ActionId}].
