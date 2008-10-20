-module(interp).
-export([
        interpret_blocks/2,
        interpret_blocks/3,
        rpc/2
    ]).


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
    Interp ! {SessionPid, AmiList}.

rpc(Interp, Command) ->
    Interp ! {self(), Command},
    receive
        {Interp, Response} ->
            Response
    end.
