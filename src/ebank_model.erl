-module(ebank_model).

%% API functions
-export([ schema/1
        , fields/1
        , fields_name/1
        , field_by_name/2
        , field_index/2
        , get_field_value/3
        , set_field_value/4
        ]).

%% Types
-export_type([ schema/0
             , field/0
             ]).

-type schema() :: #{ fields => field() }.

-type field() :: map().

%% Callbacks
-optional_callbacks([]).

-callback schema() -> schema().

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

schema(Model) ->
    Model:schema().

fields(Model) ->
    maps:get(fields, schema(Model), #{}).

fields_name(Model) ->
    maps:keys(fields(Model)).

field_by_name(Name, Model) ->
    maps:get(Name, fields(Model)).

field_index(Name, Model) ->
    maps:get(index, field_by_name(Name, Model)).

get_field_value(Name, Record, Model) ->
    get_record_value(field_index(Name, Model), Record).

set_field_value(Name, Value, Record, Model) ->
    set_record_value(field_index(Name, Model), Value, Record).

%%----------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%----------------------------------------------------------------------

get_record_value(Index, Record) ->
    erlang:element(Index, Record).

set_record_value(Index, Value, Record) ->
    erlang:setelement(Index, Record, Value).
