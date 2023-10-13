-module(ebank_maps).

%% API functions
-export([ invert/1 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

invert(Map) ->
    maps:fold(fun(K, V, Acc) -> Acc#{V => K} end, #{}, Map).
