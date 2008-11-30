-module(amiclient_session).
-behaviour(gen_fsm).
-export([
        new/3,
        login_ok/1,
        login_failed/1,
        close/1,
        send_command/2
    ]).

-export([
        init/1,
        handle_event/3,
        handle_sync_event/4,
        handle_info/3,
        terminate/3,
        code_change/4
    ]).

-export([
        get_banner/2,
        get_banner/3,
        insecure/2,
        insecure/3,
        secure/2,
        secure/3
    ]).

-include("ami.hrl").

new(Client, Username, Secret) ->
    case gen_fsm:start(?MODULE, [Client, Username, Secret], []) of
        {ok, Pid} ->
            gen_tcp:controlling_process(Client, Pid),
            case gen_fsm:sync_send_event(Pid, owner, 5000) of
                {ok, Interp} ->
                    {Pid, Interp};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

login_ok(Pid) ->
    gen_fsm:send_event(Pid, {self(), {login, ok}}).

login_failed(Pid) ->
    gen_fsm:send_event(Pid, {self(), {error, {login, failed}}}).

send_command(Pid, Cmd) ->
    gen_fsm:send_event(Pid, {self(), Cmd}).

close(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, close).

init([Client, Username, Secret]) ->
    inet:setopts(Client, [{active, once}]),
    {ok, get_banner, {Client, "", {Username, Secret, owner_not_set}}}.


handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(close, _From, _StateName, State) ->
    {stop, normal, {ok, closed}, State};

handle_sync_event(Event, _From, StateName, State) ->
    {reply, {illegal_event, Event}, StateName, State}.

handle_info({tcp, Client, NewData}, get_banner, {Client, OldData, {Username, Secret, Owner}=Extra}=_State) ->
    inet:setopts(Client, [{active, once}]),
    Data = string:concat(OldData, NewData),
    case string:str(Data, "\r\n") of
        0 ->
            {next_state, get_banner, {Client, Data, Extra}};
        _Index -> 
            "Asterisk Call Manager/" ++ _Version = util:strip(Data), 
            Request = [{action, "login"}, {username, Username}, {secret, Secret}],
            amitcp:send(Client, Request),
            Interp = amiclient_interp:new(),
            {next_state, insecure, {Client, "", {Interp, Owner}}}
    end;

handle_info({tcp, Client, NewData}, insecure, {Client, _OldData, {Interp, _Owner}}=State) ->
    inet:setopts(Client, [{active, once}]),
    interpret_data(Interp, NewData, insecure, State);

handle_info({tcp, Client, NewData}, secure, {Client, _OldData, Interp}=State) ->
    inet:setopts(Client, [{active, once}]),
    interpret_data(Interp, NewData, secure, State);

handle_info({tcp_error, Client, Reason}, _StateName, {Client, _OldData, _Extra}=State) -> 
    {stop, Reason, State};
handle_info({tcp_close, Client}, _StateName, {Client, _OldData, _Extra}=State) -> 
    {stop, connect_closed_by_peer, State};
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, {Client, _OldData, _Extra}) ->
    gen_tcp:close(Client),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {next_state, StateName, State}.


get_banner(_Event, State) ->
    {next_state, get_banner, State}.

get_banner(owner, From, {Client, OldData, {Username, Secret, _Owner}}=_State) ->
    {next_state, get_banner, {Client, OldData, {Username, Secret, From}}};
get_banner(Event, _From, State) ->
    {reply, {illegal_event, Event}, get_banner, State}.



insecure({Interp, {login, ok}}, {Client, OldData, {Interp, owner_not_set}}=_State) ->
    {next_state, secure, {Client, OldData, Interp}};
insecure({Interp, {login, ok}}, {Client, OldData, {Interp, Owner}}=_State) ->
    gen_fsm:reply(Owner, {ok, Interp}),
    {next_state, secure, {Client, OldData, Interp}};

insecure({Interp, {error, {login, failed}}}, {_Client, _OldData, {Interp, owner_not_set}}=State) ->
    {stop, normal, State};
insecure({Interp, {error, {login, failed}}}, {_Client, _OldData, {Interp, Owner}}=State) ->
    gen_fsm:reply(Owner, {error, {login, failed}}),
    {stop, normal, State};
insecure(_Event, State) ->
    {next_state, insecure, State}.

insecure(owner, From, {Client, OldData, {Interp, _Owner}}=_State) ->
    {next_state, insecure, {Client, OldData, {Interp, From}}};
insecure(Event, _From, State) ->
    {reply, {illegal_event, Event}, insecure, State}.

secure({Interp, [{action, _Action} | _Rest]=Command}, {Client, _OldData, Interp}=State) ->
    amitcp:send(Client, Command),
    {next_state, secure, State};

secure(_Event, State) ->
    {next_state, secure, State}.

secure({Interp, {close, Reason}}, From, {_Client, _OldData, Interp}=State) ->
    gen_fsm:reply(From, {ok, closed}),
    {stop, Reason, State};

secure(owner, _From, {_Client, _OldData, Interp}=State) ->
    {next_state, {ok, Interp}, secure, State};
secure(Event, _From, State) ->
    {reply, {illegal_event, Event}, secure, State}.



interpret_data(Interp, NewData, StateName, {Client, OldData, Extra}=_State) ->
    Data = string:concat(OldData, NewData),
    {BlockList, RemainingData} = messaging:get_blocks(Data),
    interp:interpret_blocks(Interp, BlockList),
    {next_state, StateName, {Client, RemainingData, Extra}}.

