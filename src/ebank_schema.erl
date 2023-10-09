-module(ebank_schema).

%% API functions
-export([ new/1
        , fields/1
        , fields_name/1
        , field/2
        , field_index/2
        , get_field_value/3
        , set_field_value/4
        ]).

%% Types
-export_type([ t/0, fields/0, field_name/0 ]).

-record(schema, { fields = #{} :: fields() }).

-opaque t() :: #schema{}.

-type fields() :: #{
    field_name() := ebank_fields:t()
}.

-type field_name() :: atom().

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

new(Args) ->
    #schema{ fields = normalize_fields(maps:get(fields, Args)) }.

fields(#schema{fields = Fields}) ->
    Fields.

fields_name(Schema) ->
    maps:keys(fields(Schema)).

field(Name, Schema) ->
    maps:get(Name, fields(Schema)).

field_index(Name, Schema) ->
    ebank_field:index(field(Name, Schema)).

get_field_value(Name, Record, Schema) ->
    ebank_records:get_value(field_index(Name, Schema), Record).

set_field_value(Name, Value, Record, Schema) ->
    ebank_records:set_value(field_index(Name, Schema), Value, Record).

%%----------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%----------------------------------------------------------------------

normalize_fields(Fields) ->
    maps:map(fun(Name, Defs) ->
        ebank_field:new(Defs#{name => Name})
    end, Fields).
