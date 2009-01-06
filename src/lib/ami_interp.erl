-module(ami_interp).
-export([behaviour_info/1]).




behaviour_info(callbacks) ->
    [
        {new,0},
        {secure, 2},
        {secure, 3},
        {insecure, 2},
        {insecure, 3}
    ];
behaviour_info(_Other) ->
    undefined.
