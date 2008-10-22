-module(amisym_eventbus).
-export([
        start/0,
        stop/0,
        init/0,
        connect/0,
        disconnect/0,
        message/1,
        is_connected/0
    ]).


start() ->
    case whereis(amisym_eventbus) of
        undefined -> 
            register(amisym_eventbus, spawn_link(?MODULE, init, [])),
            {ok, started};
        _ ->
            {ok, already_running}
    end.

stop() ->
    rpc({cmd, stop}).


rpc(Command) ->
    case whereis(amisym_eventbus) of
        undefined ->
            {error, not_running};
        _ ->
            amisym_eventbus ! {self(), Command},
            receive
                Response ->
                    Response
            end
    end.

connect() ->
    rpc({cmd, connect}).

disconnect() ->
    rpc({cmd, disconnect}).

message(Message) ->
    rpc({cmd, {message, Message}}).

is_connected() ->
    rpc({cmd, is_connected}).

init() ->
    process_flag(trap_exit, true),
    Interps = ets:new(amisym_interps, [bag, private]),
    loop(Interps).

loop(Interps) ->
    receive
        {'EXIT', _Interp, killed} ->
            just_die;
        {'EXIT', Interp, Reason} ->
            util:logmessage("~p disconnected. Reason: ~p~n", [Interp, Reason]),
            ets:delete(Interps, {interp, Interp}),
            loop(Interps);
        {From, {cmd, disconnect}} ->
            ets:delete_object(Interps, {interp, From}),
            From ! {ok, deleted},
            loop(Interps);
        {From, {cmd, connect}} ->
            ets:insert(Interps, {interp, From}),
            From ! {ok, connected},
            loop(Interps);
        {From, {cmd, {message, Message}}} ->
            AllInterps = ets:lookup(Interps, interp),
            lists:foreach(fun({interp, Interp}) -> Interp ! {amisym_eventbus, Message} end, AllInterps),
            From ! {ok, sent},
            loop(Interps);
        {From, {cmd, is_connected}} ->
            case length(ets:match_object(Interps, {interp, From})) of
                0 ->
                    From ! {ok, false},
                    loop(Interps);
                _ ->
                    From ! {ok, true},
                    loop(Interps)
            end;
        {From, {cmd, stop}} ->
            From ! {ok, stopped};
        _Any ->
            loop(Interps)
    end.
