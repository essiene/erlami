-module(amisym_server).

-behaviour(gen_listener_tcp).

-include("ami.hrl").

-export([
        start/0,
        start/1,
        stop/1,
        serve/2
    ]).

-export([
            init/1
        ]).


start() ->
    gen_listener_tcp:start({local, ?LISTENER}, ?MODULE, [erlcfg:new()], []). 

start(Port) when is_integer(Port) ->
    gen_listener_tcp:start({local, ?LISTENER}, ?MODULE, [Port, 10], []);

start(Config) when is_tuple(Config) ->
    gen_listener_tcp:start_link({local, ?LISTENER}, ?MODULE, [Config], []).

init([{_Port, _Backlog}]) ->
    ok;

init([Config]) ->
    {ok, {
            Config:get(server.port, 15038),
            [
                binary,
                inet, 
                {active, false},
                {backlog, config:get(server.backlog, 10)},
                {reuseaddr, true}
            ],
            
            {
                amisym_client_sup,
                start_child,
                []
            }
         }
    }.
                
serve(Server, SessionList) ->
    receive
        close ->
            lists:foreach(
                fun(S) -> amisym_session:close(S) end,
                SessionList
            ),
            exit(normal);
        _Any ->
            do_nothing
    after 0 ->
        try
            Client = amitcp:wait_for_connection(Server),
            Session = amisym_session:new(Client),
            NewSessionList = [Session | SessionList],
            serve(Server, NewSessionList)
        catch
            Type: Exception ->
                {Type, Exception}
        after
            gen_tcp:close(Server)
        end
   end.
