-module(ebank_converter).

%% API functions
-export([ binary_to_integer/1 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

binary_to_integer(Bin) ->
    try
        {ok, erlang:binary_to_integer(Bin)}
    catch
        error:badarg ->
            {error, badarg}
    end.
