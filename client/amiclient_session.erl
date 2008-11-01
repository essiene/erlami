-module(amiclient_session).
-export([
        new/3,
        state_no_banner/5,
        state_not_logged_in/6,
        state_normal/3
    ]).

-include("ami.hrl").

new(Client, Username, Secret) ->
    Owner = self(),
    Pid = spawn_link(?MODULE, state_no_banner, [Owner, Client, Username, Secret, ""]),
    gen_tcp:controlling_process(Client, Pid),
    receive
        {'EXIT', Pid, Reason} ->
            {error, Reason};
        {Pid, {ok, Interp}} ->
            {Pid, Interp};
        {Pid, Reason} ->
            {error, Reason}
    after 5000 ->
            {error, connection_timeout}
    end.


state_no_banner(Owner, Client, Username, Secret, OldData) ->
    inet:setopts(Client, [{active, once}]),
    receive
        {tcp, Client, NewData} ->
            Data = string:concat(OldData, NewData),
            case string:str(Data, "\r\n") of
                0 ->
                    state_no_banner(Owner, Client, Username, Secret, Data);
                _ -> 
                    "Asterisk Call Manager/1.0" = util:strip(Data),
                    Request = [{action, "login"}, {username, Username}, {secret, Secret}],
                    amitcp:send(Client, Request),
                    Interp = amiclient_interp:new(),
                    state_not_logged_in(Owner, Client, Interp, Username, Secret, "")
            end;
        {tcp_close, Client} ->
            exit(connect_closed_by_peer);
        {tcp_error, Client, Reason} ->
            exit(Reason);
        _Any ->
            state_no_banner(Owner, Client, Username, Secret, OldData)
    end.

state_not_logged_in(Owner, Client, Interp, Username, Secret, OldData) ->
    inet:setopts(Client, [{active, once}]),
    receive
        {Interp, {login, ok}} ->
            Owner ! {self(), {ok, Interp}},
            state_normal(Client, Interp, OldData);
        {Interp, {login, failed}} ->
            Owner ! {self(), {login, failed}};
        {tcp, Client, NewData} ->
            Data = string:concat(OldData, NewData),
            {BlockList, RemainingData} = messaging:get_blocks(Data),
            interp:interpret_blocks(Interp, BlockList),
            state_not_logged_in(Owner, Client, Interp, Username, Secret, RemainingData);
        {tcp_close, Client} ->
            exit(connect_closed_by_peer);
        {tcp_error, Client, Reason} ->
            util:logmessage(Reason),
            state_not_logged_in(Owner, Client, Interp, Username, Secret, OldData);
        Any ->
            util:logmessage(Any),
            state_not_logged_in(Owner, Client, Interp, Username, Secret, OldData)
    end.

state_normal(Client, Interp, OldData) ->
    inet:setopts(Client, [{active, once}]),
    receive
        {Interp, {close, Reason}} ->
            util:logmessage(Reason),
            Interp ! {self(), {ok, closed}};
        {Interp, [{action, _Action} | _Rest] = Command} ->
            amitcp:send(Client, Command),
            state_normal(Client, Interp, OldData);
        {tcp, Client, NewData} ->
            Data = string:concat(OldData, NewData),
            {BlockList, RemainingData} = messaging:get_blocks(Data),
            interp:interpret_blocks(Interp, BlockList),
            state_normal(Client, Interp, RemainingData);
        {tcp_closed, Client} ->
            gen_tcp:close(Client),
            exit(connect_closed_by_peer);
        {tcp_error, Client, Reason} ->
            util:logmessage(Reason),
            state_normal(Client, Interp, OldData);
        Any ->
            util:logmessage(Any),
            state_normal(Client, Interp, OldData)
    end.
