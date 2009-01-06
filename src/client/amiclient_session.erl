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
    ClientSession = #client_session{conn=Client, username=Username, secret=Secret},
    {ok, get_banner, ClientSession}.


handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(close, _From, _StateName, State) ->
    {stop, normal, {ok, closed}, State};

handle_sync_event(Event, _From, StateName, State) ->
    {reply, {illegal_event, Event}, StateName, State}.

handle_info({tcp, Client, NewData}, get_banner, #client_session{conn=Client}=St) ->
    inet:setopts(Client, [{active, once}]),
    Data = string:concat(St#client_session.data, NewData),
    case string:str(Data, "\r\n") of
        0 ->
            {next_state, get_banner, St#client_session{data=Data}};
        _Index -> 
            "Asterisk Call Manager/" ++ _Version = util:strip(Data), 
            Request = [{action, "login"}, {username, St#client_session.username}, {secret, St#client_session.secret}],
            amitcp:send(Client, Request),
            Interp = amiclient_interp:new(),
            {next_state, insecure, St#client_session{data="", interp=Interp}}
    end;

handle_info({tcp, Client, NewData}, StateName, #client_session{conn=Client}=St) ->
    inet:setopts(Client, [{active, once}]),
    interpret_data(St, NewData, StateName);

handle_info({tcp_error, Client, Reason}, _StateName, #client_session{conn=Client}=St) -> 
    {stop, Reason, St};
handle_info({tcp_close, Client}, _StateName, #client_session{conn=Client}=St) -> 
    {stop, connect_closed_by_peer, St};
handle_info(_Info, StateName, St) ->
    {next_state, StateName, St}.

terminate(_Reason, _StateName, St) ->
    gen_tcp:close(St#client_session.conn),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {next_state, StateName, State}.


get_banner(_Event, St) ->
    {next_state, get_banner, St}.

get_banner(owner, From, St) ->
    {next_state, get_banner, St#client_session{owner=From}};
get_banner(Event, _From, St) ->
    {reply, {illegal_event, Event}, get_banner, St}.



insecure({Interp, {login, ok}}, #client_session{interp=Interp, owner=undefined}=St) ->
    {next_state, secure, St};
insecure({Interp, {login, ok}}, #client_session{interp=Interp}=St) ->
    gen_fsm:reply(St#client_session.owner, {ok, Interp}),
    {next_state, secure, St};

insecure({Interp, {error, {login, failed}}}, #client_session{interp=Interp, owner=undefined}=St) ->
    {stop, normal, St};
insecure({Interp, {error, {login, failed}}}, #client_session{interp=Interp}=St) ->
    gen_fsm:reply(St#client_session.owner, {error, {login, failed}}),
    {stop, normal, St};
insecure(_Event, St) ->
    {next_state, insecure, St}.

insecure(owner, From, St) ->
    {next_state, insecure, St#client_session{owner=From}};
insecure(Event, _From, St) ->
    {reply, {illegal_event, Event}, insecure, St}.

secure({Interp, [{action, _Action} | _Rest]=Command}, #client_session{interp=Interp}=St) ->
    amitcp:send(St#client_session.conn, Command),
    {next_state, secure, St};

secure(_Event, St) ->
    {next_state, secure, St}.

secure({Interp, {close, Reason}}, From, #client_session{interp=Interp}=St) ->
    gen_fsm:reply(From, {ok, closed}),
    {stop, Reason, St};

secure(owner, _From, St) ->
    {next_state, {ok, St#client_session.interp}, secure, St};
secure(Event, _From, St) ->
    {reply, {illegal_event, Event}, secure, St}.

interpret_data(St, NewData, StateName) ->
    Data = string:concat(St#client_session.data, NewData),
    {BlockList, RemainingData} = messaging:get_blocks(Data),
    interp:interpret_blocks(St#client_session.interp, BlockList),
    {next_state, StateName, St#client_session{data=RemainingData}}.
