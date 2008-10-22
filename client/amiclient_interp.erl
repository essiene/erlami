-module(amiclient_interp).
-export([
        new/0,
        state_not_logged_in/1,
        state_logged_in/3
    ]).


new() ->
    SessionPid = self(),
    spawn_link(?MODULE, state_not_logged_in, [SessionPid]).

state_not_logged_in(SessionPid) ->
    receive
        {SessionPid, {cmd, exit}} ->
            do_nothing;
        {SessionPid, [{response, "Success"} | _Rest]} ->
            SessionPid ! {self(), {login, ok}},
            state_logged_in(SessionPid, 0, ets:new(actionid_pidmap, [private]));
        {SessionPid, [{response, "Error"} | _Rest]} ->
            SessionPid ! {self(), {login, failed}};
        _Any ->
            state_not_logged_in(SessionPid)
    end.

state_logged_in(SessionPid, Tid, Ets) ->
    receive 
        {SessionPid, [{response, _Status} | _Rest] = Response} ->
            ActionId = amilist:get_value(Response, actionid),
            [{ActionId, SenderPid}] = ets:lookup(Ets, ActionId),
            SenderPid ! {self(), Response},
            ets:delete(Ets, ActionId),
            state_logged_in(SessionPid, Tid, Ets);
        {From, close} ->
            SessionPid ! {self(), {close, close_requested}},
            From ! {ok, closed};
        {From, [{action, _Action} | _Rest] = Command} ->
            ActionId = Tid + 1,
            NewCommand = amilist:set_value(Command, actionid, ActionId),
            SessionPid ! {self(), NewCommand},
            ets:insert(Ets, {integer_to_list(ActionId), From}),
            state_logged_in(SessionPid, ActionId, Ets);
        {_From, [{event, _EventName} | _Rest] = Event} ->
            util:logmessage(Event),
            util:logmessage("----"),
            state_logged_in(SessionPid, Tid, Ets);
        _Any ->
            state_logged_in(SessionPid, Tid, Ets)
    end.
