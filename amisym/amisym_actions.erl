-module(amisym_actions).
-export([
        action/3,
        a_login/2,
        a_logout/2,
        a_not_logged_in/2,
        a_command/2
    ]).
-include("ami.hrl").




action(Action, Command, IsLoggedIn) ->
    try
        Function = list_to_atom(?SYM_ACTION_FUNCTION_PREFIX ++ Action),
        apply(?MODULE, Function, [Command, IsLoggedIn])
    catch
        error: undef->
            [{response, "Error"}, {message, "No such action"}]
    end.

a_login(_Command, true) ->
    [{response, "Success"}, {message, "Already logged in"}];
a_login(Command, false) ->
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

a_logout(_Command, true) ->
    [{response, "Success"}, {message, "Logged out"}].

a_not_logged_in(_Command, _) ->
    [{resonse, "Error"}, {message, "Not logged in"}].

a_command(_Command, true) ->
    [{response, "Success"}, {message, "AMISym 0.1"}].
