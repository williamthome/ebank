-module(ebank_schema).

%% API functions
-export([ new/1
        , table/1
        , fields/1
        , fields_name/1
        , indexed_fields_name/1
        , fields_index/1
        , fields_type/1
        , permitted_fields/1
        , required_fields/1
        , field/2
        , field_index/2
        , get_field_value/3
        , set_field_value/4
        , changeset/3
        , changeset/4
        , to_record/2
        ]).

%% Types
-export_type([ t/0, table/0, fields/0, field_name/0, changeset/0, data/0 ]).

-record(schema, { table :: table()
                , fields :: fields()
                }).

-opaque t() :: #schema{}.

-type table() :: ebank_db:table().
-type fields() :: #{ field_name() := ebank_fields:t() }.
-type field_name() :: atom().
-type changeset() :: changeset:t().
-type data() :: map().

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

new(Args) ->
    #schema{ table = maps:get(table, Args)
           , fields = normalize_fields(maps:get(fields, Args))
           }.

table(#schema{table = Table}) ->
    Table.

fields(#schema{fields = Fields}) ->
    Fields.

fields_name(Schema) ->
    lists:map(fun({Name, _}) -> Name end, fields(Schema)).

indexed_fields_name(Schema) ->
    lists:filtermap(fun({Name, Field}) ->
        case ebank_field:indexed(Field) of
            true -> {true, Name};
            false -> false
        end
    end, fields(Schema)).

fields_index(Schema) ->
    InitialIndex = 1,
    element(1, lists:foldl(fun(Name, {Acc, Index}) ->
        {Acc#{Name => Index}, Index+1}
    end, {#{}, InitialIndex}, fields_name(Schema))).

fields_type(Schema) ->
    lists:foldl(fun({Name, Field}, Acc) ->
        Acc#{Name => ebank_field:type(Field)}
    end, #{}, fields(Schema)).

permitted_fields(Schema) ->
    lists:filtermap(fun({Name, Field}) ->
        case ebank_field:permitted(Field) of
            true -> {true, Name};
            false -> false
        end
    end, fields(Schema)).

required_fields(Schema) ->
    lists:filtermap(fun({Name, Field}) ->
        case ebank_field:required(Field) of
            true -> {true, Name};
            false -> false
        end
    end, fields(Schema)).

field(Name, Schema) ->
    {Name, Field} = proplists:lookup(Name, fields(Schema)),
    Field.

field_index(Name, Schema) ->
    ebank_field:index(field(Name, Schema)).

get_field_value(Name, Record, Schema) ->
    ebank_records:get_value(field_index(Name, Schema), Record).

set_field_value(Name, Value, Record, Schema) ->
    ebank_records:set_value(field_index(Name, Schema), Value, Record).

changeset(Data, Params, Schema) ->
    Required = required_fields(Schema),
    Pipes = [ changeset:validate_required(Required) ],
    changeset(Data, Params, Schema, Pipes).

changeset(Data, Params, Schema, Pipes) ->
    Types = fields_type(Schema),
    Permitted = permitted_fields(Schema),
    Changeset = changeset:cast({Data, Types}, Params, Permitted),
    changeset:pipe(Changeset, Pipes).

to_record(Params, Schema) ->
    ebank_records:from_map(Params, fields_name(Schema), table(Schema)).

%%----------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%----------------------------------------------------------------------

normalize_fields(Fields) ->
    element(1, lists:mapfoldl(fun({Name, Args}, I) ->
        {normalize_field({Name, Args}, I), I+1}
    end, 1, Fields)).

normalize_field({Name, Args0}, Index) ->
    Args = Args0#{name => Name, index => Index},
    {Name, ebank_field:new(Args)}.
