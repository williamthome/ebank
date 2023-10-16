-module(ebank_maps).

%% API functions
-export([ invert/1, binary_keys_to_existing_atom/1 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

invert(Map) ->
    maps:fold(fun(K, V, Acc) -> Acc#{V => K} end, #{}, Map).

binary_keys_to_existing_atom(Map) ->
    maps:fold(fun(Key, Val, Acc) ->
        Acc#{binary_to_existing_atom(Key) => Val}
    end, #{}, Map).
