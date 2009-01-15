-module(amisymctl).
-export([
        action/0,
        action/1
    ]).


action([exit]) ->
    display(
        amisym_exec(init, stop, [], exited)
    );

action([stop]) ->
    display(
        amisym_exec(amisym_app, stop, [], stopped)
    );

action([start]) ->
    display(
        amisym_exec(amisym_app, start, [], started)
    );

action([restart]) ->
    action([stop]),
    action([start]);

action([status]) ->
    display(
        amisym_exec(amisym_app, status, [])
    );

action(Any) ->
    available_commands(Any).

action() ->
    action([status]).

available_commands(_WrongCommand) ->
    OutString = "amisymctl [actions]\n" 
    "actions:\n" 
    "   exit    - Takes down a amisym node.\n"
    "   stop    - Stops the running amisym instance, without\n"
    "             bringing down the node.\n"
    "   start   - Start amisym on an alive node.\n"
    "   status  - Report the status of an alive node.\n"
    "   restart - Restart amisym on an alive node.",
    io:format("~s\n", [OutString]).

display([]) ->
    do_nothing;
display({Key, Val}) ->
    display(Key),
    display(" - ["),
    display(Val),
    display("]");
display([{_Key, _Val}=Head | Rest]) ->
    display(Head),
    display("\n"),
    display(Rest);
display(Atom) when is_atom(Atom) ->
    io:format("~p", [Atom]);
display(String) when is_list(String) ->
    io:format("~s", [String]).

amisym_exec(Module, Fun, Args) ->
    AmisymNode = get_amisym_node(),
    case rpc:call(AmisymNode, Module, Fun, Args) of
        {badrpc, node_down} ->
            [{amisym, dead}];
        {badrpc, _Reason} ->
            [{amisym, stopped}];        
        Response ->
            Response
    end.

amisym_exec(Module, Fun, Args, OkResponse) ->
    case amisym_exec(Module, Fun, Args) of
        [{amisym, _Reason}] = Response ->
            Response;
        _Other ->
            [{amisym, OkResponse}]
    end.


get_amisym_node() ->
    [AmisymNode] = init:get_plain_arguments(),
    list_to_atom(AmisymNode).
