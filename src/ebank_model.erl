-module(ebank_model).

%% API functions
-export([ new/1
        , module/1
        , schema/1
        , fields/1
        , fields_name/1
        , field/2
        , field_index/2
        , get_field_value/3
        , set_field_value/4
        ]).

%% Types
-export_type([ t/0, schema/0, field/0 ]).

-record(model, { module :: module()
               , schema :: schema()
               }).

-opaque t() :: #model{}.

-type schema() :: ebank_schema:t().

-type field() :: ebank_field:t().

%% Callbacks
-optional_callbacks([]).

-callback model() -> t().

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

new(Args) ->
    #model{ module = maps:get(module, Args)
          , schema = ebank_schema:new(maps:get(schema, Args))
          }.

module(#model{module = Module}) ->
    Module.

schema(#model{schema = Schema}) ->
    Schema.

fields(Model) ->
    ebank_schema:fields(schema(Model)).

fields_name(Model) ->
    ebank_schema:fields_name(schema(Model)).

field(Name, Model) ->
    ebank_schema:field(Name, schema(Model)).

field_index(Name, Model) ->
    ebank_schema:field_index(Name, schema(Model)).

get_field_value(Name, Record, Model) ->
    ebank_schema:set_field_value(Name, Record, schema(Model)).

set_field_value(Name, Value, Record, Model) ->
    ebank_schema:set_field_value(Name, Value, Record, schema(Model)).
