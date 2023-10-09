-module(ebank_records).

%% API functions
-export([ get_value/2, set_value/3 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

get_value(Index, Record) ->
    erlang:element(Index, Record).

set_value(Index, Value, Record) ->
    erlang:setelement(Index, Record, Value).
