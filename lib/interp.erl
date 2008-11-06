-module(interp).
-export([
        interpret_blocks/2,
        interpret_blocks/3,
        rpc/2,
        rpc/3,
        cast/2
    ]).

-export([
        change_state/1,
        close/1
    ]).

change_state(Interp) ->
    gen_fsm:send_event(Interp, {self(), change_state}).

close(Interp) ->
    gen_fsm:send_event(Interp, {self(), close}).

interpret_blocks(Interp, ListOfBlocks) ->
    SessionPid = self(),
    spawn(?MODULE, interpret_blocks, [Interp, ListOfBlocks, SessionPid]).

interpret_blocks(_Interp, [], _SessionPid) -> 
    true;
interpret_blocks(Interp, [Block | Rest], SessionPid) ->
    AmiList = messaging:block_to_amilist(Block),
    case amilist:has_key(AmiList, action) of
        true ->
            interpret_amilist(Interp, AmiList, SessionPid);
        false ->
            case amilist:has_key(AmiList, event) of
                true ->
                    interpret_amilist(Interp, AmiList, SessionPid);
                false ->
                    case amilist:has_key(AmiList, response) of
                        true ->
                            interpret_amilist(Interp, AmiList, SessionPid);
                        false ->
                            util:logmessage("****Invalid Message****"),
                            util:logmessage(AmiList),
                            util:logmessage("***********************")
                    end
            end
    end,
    interpret_blocks(Interp, Rest, SessionPid).


interpret_amilist(Interp, AmiList, SessionPid) ->
    gen_fsm:send_event(Interp, {SessionPid, AmiList}).

cast(Interp, Command) ->
    gen_fsm:send_event(Interp, Command).

rpc(Interp, Command) ->
    gen_fsm:sync_send_event(Interp, Command).
rpc(Interp, Command, Timeout) ->
    gen_fsm:sync_send_event(Interp, Command, Timeout).
