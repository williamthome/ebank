-module(ebank_records).

%% API functions
-export([ get_value/2, set_value/3, from_map/3, to_map/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

get_value(Index, RecordName) ->
    erlang:element(Index, RecordName).

set_value(Index, Value, RecordName) ->
    erlang:setelement(Index, RecordName, Value).

from_map(Params, FieldsName, RecordName) ->
    list_to_tuple(
        [RecordName |
            lists:map(fun(FieldName) ->
                maps:get(FieldName, Params, undefined)
            end, FieldsName)
        ]
    ).

to_map(Record, IndexesName) ->
    element(1, lists:foldl(fun(Val, {Acc, Index}) ->
        FieldName = maps:get(Index, IndexesName),
        {Acc#{FieldName => Val}, Index+1}
    end, {#{}, 1}, tl(tuple_to_list(Record)))).

%%----------------------------------------------------------------------
%% TESTS
%%----------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

to_map_test() ->
    Expected = #{name => <<"Joe">>, age => 68},
    IndexesName = #{1 => name, 2 => age},
    Record = {user, <<"Joe">>, 68},
    ?assertEqual(Expected, to_map(Record, IndexesName)).

-endif.
