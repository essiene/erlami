-module(ami).
-author("Essien Ita Essien <essien.ita@uknglobal.com>").
-export([
        new/2,
        new/4,
        init/2,
        init/4,
        send_cmd/1,
        originate/4
    ]).


new(Host, Port, Username, Secret) ->
    register(ami_recv, spawn(?MODULE, init, [Host, Port, Username, Secret])).
new(Username, Secret) ->
    register(ami_recv, spawn(?MODULE, init, [Username, Secret])).

init(Username, Secret) ->
    init("localhost", 5038, Username, Secret).

init(Host, Port, Username, Secret) ->
    case connect(Host, Port, Username, Secret) of
        {error, Reason} ->
            {error, Reason};
        {ok, Socket} ->
            ami_msg:start(Socket),
            inet:setopts(Socket, [{active, true}]),
            ami_recv:loop("")
    end.


originate(Channel, Context, Extension, Priority) ->
    send_cmd([
            {action, "originate"},
            {channel, Channel},
            {context, Context},
            {exten, Extension},
            {pRiority, Priority}
        ]).

send_cmd(Command) ->
    case ami_msg:send(Command) of
        {ok, _} -> 
            get_response();
        Other ->
            Other
    end.


get_response() ->
    receive
        Message ->
            Message
    end.


