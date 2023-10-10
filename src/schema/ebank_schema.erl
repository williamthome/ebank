-module(ebank_schema).

%% API functions
-export([ new/1
        , table/1
        , record/1
        , fields_iterator/1
        , fields/1
        , fields_name/1
        , indexed_fields_name/1
        , fields_default/1
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

fields_default(Schema) ->
    maps:fold(fun(Name, Field, Acc) ->
        case ebank_field:default(Field) of
            undefined -> Acc;
            Default -> Acc#{Name => Default}
        end
    end, #{}, fields(Schema)).

fields_type(Schema) ->
    maps:map(fun(_, Field) ->
        ebank_field:type(Field)
    end, fields(Schema)).

permitted_fields(Schema) ->
    maps:fold(fun(Name, Field, Acc) ->
        case ebank_field:permitted(Field) of
            true -> [Name | Acc];
            false -> Acc
        end
    end, [], fields(Schema)).

required_fields(Schema) ->
    maps:fold(fun(Name, Field, Acc) ->
        case ebank_field:required(Field) of
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

changeset(Data, Params, Schema) ->
    Required = required_fields(Schema),
    Defaults = fields_default(Schema),
    Pipes = [ changeset:validate_required(Required)
            , pipe_apply_defaults(Defaults)
            ],
    changeset(Data, Params, Schema, Pipes).

changeset(Data, Params, Schema, Pipes) ->
    Types = fields_type(Schema),
    Permitted = permitted_fields(Schema),
    Changeset = changeset:cast({Data, Types}, Params, Permitted),
    changeset:pipe(Changeset, Pipes).

to_record(Params, Schema) ->
    ebank_records:from_map(Params, fields_iterator(Schema), record(Schema)).

%%----------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%----------------------------------------------------------------------

pipe_apply_defaults(Defaults) ->
    fun(Changeset) ->
        Data = changeset:get_data(Changeset),
        Changes = changeset:get_changes(Changeset),
        EmptyValues = changeset:get_empty_values(Changeset),
        maps:fold(fun(FieldName, DefaultFun, Acc) ->
            IsDefined = changeset:is_field_value_defined(FieldName, Data, Changes, EmptyValues),
            case IsDefined of
                true ->
                    Acc;
                false ->
                    DefaultFun(Acc)
            end
        end, Changeset, Defaults)
    end.

normalize_fields(Fields) ->
    maps:iterator(element(1, maps:fold(fun(Name, Args0, {FAcc, I}) ->
        Args = Args0#{name => Name, index => I},
        {FAcc#{Name => ebank_field:new(Args)}, I+1}
    end, {#{}, 2}, Fields)), ordered).
