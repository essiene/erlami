-module(amisym_commands).
-export([
        login/2,
        logout/2,
        not_logged_in/2,
        command/2
    ]).


login(_Command, true) ->
    [{response, "Success"}, {message, "Already logged in"}];
login(Command, false) ->
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

logout(_Command, true) ->
    [{response, "Success"}, {message, "Logged out"}].

not_logged_in(_Command, _) ->
    [{resonse, "Error"}, {message, "Not logged in"}].

command(Command, true) ->
    ActionId = amilist:get_value(Command, actionid),
    [{response, "Success"}, {message, "AMISym 0.1"}, {actionid, ActionId}].
