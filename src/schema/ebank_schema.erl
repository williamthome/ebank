-module(ebank_schema).

%% API functions
-export([ new/1
        , table/1
        , record/1
        , fields_iterator/1
        , fields/1
        , fields_name/1
        , indexed_fields_name/1
        , field/2
        , field_index/2
        , get_field_value/3
        , set_field_value/4
        , changeset/2
        , changeset/3
        , to_record/2
        ]).

%% Types
-export_type([ t/0, fields/0, field_name/0 ]).

-record(schema, { table :: table()
                , record :: record()
                , fields :: fields()
                }).

-opaque t() :: #schema{}.

-type table() :: ebank_db:table().

-type record() :: atom() | table().

-type fields() :: #{ field_name() := ebank_fields:t() }.

-type field_name() :: atom().

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

new(Args) ->
    #schema{ table = maps:get(table, Args)
           , record = maps:get(record, Args, maps:get(table, Args))
           , fields = normalize_fields(maps:get(fields, Args))
           }.

table(#schema{table = Table}) ->
    Table.

record(#schema{record = Record}) ->
    Record.

fields_iterator(#schema{fields = Iterator}) ->
    Iterator.

fields(#schema{fields = [_|Fields]}) ->
    Fields.

fields_name(#schema{fields = [Name|_]}) ->
    Name.

indexed_fields_name(Schema) ->
    maps:fold(fun(Name, Field, Acc) ->
        case ebank_field:indexed(Field) of
            true -> [Name | Acc];
            false -> Acc
        end
    end, [], fields(Schema)).

field(Name, Schema) ->
    maps:get(Name, fields(Schema)).

field_index(Name, Schema) ->
    ebank_field:index(field(Name, Schema)).

get_field_value(Name, Record, Schema) ->
    ebank_records:get_value(field_index(Name, Schema), Record).

set_field_value(Name, Value, Record, Schema) ->
    ebank_records:set_value(field_index(Name, Schema), Value, Record).

changeset(Params, Schema) ->
    changeset(#{}, Params, Schema).

changeset(Data, Params, Schema) ->
    Fields = normalize_fields_for_changeset(fields(Schema)),
    {Types, Permitted, Required} = Fields,
    Changeset = changeset:cast({Data, Types}, Params, Permitted, #{
        metadata => Schema
    }),
    Pipes = [ changeset:validate_required(Required) ],
    changeset:pipe(Changeset, Pipes).

to_record(Params, Schema) ->
    ebank_records:from_map(Params, fields_iterator(Schema), record(Schema)).

%%----------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%----------------------------------------------------------------------

normalize_fields(Fields) ->
    maps:iterator(element(1, maps:fold(fun(Name, Args0, {FAcc, I}) ->
        Args = Args0#{name => Name, index => I},
        {FAcc#{Name => ebank_field:new(Args)}, I+1}
    end, {#{}, 2}, Fields)), ordered).

normalize_fields_for_changeset(Fields) ->
    maps:fold(fun(Name, Field, {TypesAcc, PermittedAcc, RequiredAcc}) ->
        Types = TypesAcc#{Name => ebank_field:type(Field)},
        Permitted = case ebank_field:permitted(Field) of
            true -> [Name | PermittedAcc];
            false -> PermittedAcc
        end,
        Required = case ebank_field:required(Field) of
            true -> [Name | RequiredAcc];
            false -> RequiredAcc
        end,
        {Types, Permitted, Required}
    end, {#{}, [], []}, Fields).
