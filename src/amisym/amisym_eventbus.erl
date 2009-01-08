-module(amisym_eventbus).

-behaviour(gen_server).

-define(NAME, ?MODULE).

-export([
            start_link/0,
            init/1,
            handle_call/3,
            handle_cast/2,
            handle_info/2,
            terminate/2,
            code_change/3
        ]).

-export([
            start/0,
            stop/0,
            init/0,
            connect/0,
            disconnect/0,
            message/1,
            is_connected/0
        ]).


start_link() ->
    gen_server:start_link({local, ?NAME}, ?MODULE, [], []).

init(_Arg) ->
    init().


start() ->
    case start_link() of 
        {ok, _Pid} ->
            {ok, started};
        {error, {already_started, _Pid}} ->
            {ok, already_running}
    end.   
            

stop() ->
    gen_server:call(?NAME, {stop}).

init() ->
    Interps = ets:new(amisym_interps, [bag, private]),
    {ok, Interps}.

connect() ->
    gen_server:call(?NAME, {connect, self()}).    

disconnect() ->
    gen_server:call(?NAME, {disconnect, self()}).

message(Message) ->
    gen_server:call(?NAME, {message, Message}).    

is_connected() ->
    gen_server:call(?NAME, {is_connected, self()}).

handle_call({stop}, _From, Interps) ->
    {reply, {ok, stopped}, Interps};

handle_call({connect, Pid}, _From, Interps) ->
    ets:insert(Interps, {interp, Pid}),
    {reply, {ok, connected}, Interps};

handle_call({disconnect, Pid}, _From, Interps) ->
    case ets:delete_object(Interps, {interp, Pid}) of
        true ->
            {reply, {ok, deleted}, Interps};
        false ->
            {reply, {error, not_found}, Interps}
    end;            

handle_call({message, Message}, _From, Interps) ->
    AllInterps = ets:lookup(Interps, interp),
    lists:foreach(fun({interp, Interp}) -> interp:cast(Interp, {?NAME, Message}) end, AllInterps),
    {reply, {ok, sent}, Interps};        

handle_call({is_connected, Pid}, _From, Interps) ->

    case length(ets:match_object(Interps, {interp, Pid})) of 
        0 ->
            {reply, {ok, false}, Interps};
        _ ->
            {reply, {ok, true}, Interps}
    end;            


handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, _State) ->
    ok.

terminate(_Reason, _State) ->
    ok.    

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
