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
            connect/0,
            disconnect/0,
            message/1,
            is_connected/0
        ]).


start_link() ->
    gen_server:start_link({local, ?NAME}, ?MODULE, [], []).

init(_Arg) ->
    Interps = ets:new(amisym_interps, [bag, private]),
    {ok, Interps}.

connect() ->
    gen_server:call(?NAME, connect).    

disconnect() ->
    gen_server:call(?NAME, disconnect).

message(Message) ->
    gen_server:call(?NAME, {message, Message}).    

is_connected() ->
    gen_server:call(?NAME, is_connected).

handle_call(connect, From, Interps) ->
    ets:insert(Interps, {interp, From}),
    {reply, {ok, connected}, Interps};

handle_call(disconnect, From, Interps) ->
    case ets:delete_object(Interps, {interp, From}) of
        true ->
            {reply, {ok, deleted}, Interps};
        false ->
            {reply, {error, not_found}, Interps}
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
