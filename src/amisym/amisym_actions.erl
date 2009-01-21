-module(amisym_actions).
-export([
        action/3,
        a_login/2,
        a_logout/2,
        a_not_logged_in/2,
        a_command/2,
        a_originate/2,
        originate_state_newchannel/1
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
    [{response, "Error"}, {message, "Not logged in"}].

a_command(_Command, true) ->
    [{response, "Success"}, {message, "AMISym 0.1"}].

a_originate(Command, true) ->
    try
        Channel = amilist:get_value(Command, channel),
        _Context = amilist:get_value(Command, context),
        _Extension = amilist:get_value(Command, exten),
        _Priority = amilist:get_value(Command, 'priority'),
        
        case Channel of
            "SIP/fail" ++ _Number ->
                [{response, "Error"}, {message, "Originate failed"}];
            "SIP/pass" ++ _Number ->
                originate_events("SIP/pass"),
                [{response, "Success"}, {message, "Originate successfull"}];
            {error, {no_key, channel}} ->
                [{response, "Error"}, {message, "Channel not specified"}]
        end

    catch
        _:Exception ->
            [{response, "Error"}, {message, Exception}]
    end.

originate_events(Channel) ->
    spawn(?MODULE, originate_state_newchannel, [Channel]).

originate_state_newchannel(Channel) ->
    Event = [{event, 'NewChannel'}, {state, 'Down'}, {channel, Channel ++ "-ffaabbc"}],
    amisym_eventbus:message(Event),
    timer:sleep(1000),
    originate_state_newstate_ringing(Channel).

originate_state_newstate_ringing(Channel) ->
    Event = [{event, 'NewState'}, {state, 'Ringing'}, {channel, Channel ++ "-ffaabbc"}],
    amisym_eventbus:message(Event),
    timer:sleep(5000),
    originate_state_newstate_up(Channel).

originate_state_newstate_up(Channel) ->
    Event = [{event, 'NewState'}, {state, 'Up'}, {channel, Channel ++ "-ffaabbc"}],
    amisym_eventbus:message(Event),
    timer:sleep(10000),
    originate_state_hangup(Channel).

originate_state_hangup(Channel) ->
    Event = [{event, 'Hangup'}, {channel, Channel ++ "-ffaabbc"}],
    amisym_eventbus:message(Event).
