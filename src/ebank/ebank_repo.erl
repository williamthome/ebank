-module(ebank_repo).

%% API functions
-export([ create_table/2
        , insert_one/2
        , fetch/3
        , fetch_one/3
        , update_one/3
        , normalize_data/2
        , normalize_data_list/2
        ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

create_table(Args, SchemaMod) ->
    Schema = SchemaMod:schema(),
    ebank_db:create_table(Args#{
        name => ebank_schema:table(Schema),
        fields => ebank_schema:fields_name(Schema)
    }).

insert_one(Params, SchemaMod) when is_map(Params) ->
    Schema = SchemaMod:schema(),
    Changeset = SchemaMod:changeset(#{}, Params),
    normalize_one_data_result(do_insert([Changeset], Schema)).

fetch(Query, Bindings, _SchemaMod) ->
    Fetch = fun() -> ebank_db:read(Query, Bindings) end,
    ebank_db:with_transaction(Fetch).

fetch_one(Query, Bindings, SchemaMod) ->
    normalize_one_data_result(fetch(Query, Bindings, SchemaMod)).

update_one(Record, Params, SchemaMod) ->
    Schema = SchemaMod:schema(),
    IndexesName = ebank_maps:invert(ebank_schema:fields_index(Schema)),
    Data = ebank_records:to_map(Record, IndexesName),
    Changeset = SchemaMod:changeset(Data, Params),
    normalize_one_data_result(do_update([Changeset], Schema)).

normalize_data(Data, SchemaMod) ->
    Schema = SchemaMod:schema(),
    do_normalize_data(Data, Schema).

normalize_data_list(DataList, SchemaMod) ->
    Schema = SchemaMod:schema(),
    lists:map(fun(Data) ->
        do_normalize_data(Data, Schema)
    end, DataList).

%%----------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%----------------------------------------------------------------------

normalize_one_data_result({ok, [Data]}) ->
    {ok, Data};
normalize_one_data_result({ok, []}) ->
    {error, enoent};
normalize_one_data_result({error, Reason}) ->
    {error, Reason}.

do_insert(Changesets, Schema) ->
    Table = ebank_schema:table(Schema),
    Insert = fun() ->
        DataList = lists:map(fun(Changeset) ->
            case changeset:is_valid(Changeset) of
                true ->
                    Changes = changeset:get_changes(Changeset),
                    Record = ebank_schema:to_record(Changes, Schema),
                    case ebank_db:write(Record, Table) of
                        ok ->
                            Record;
                        {error, Reason} ->
                            ebank_db:abort_transaction({error, Reason})
                    end;
                false ->
                    ebank_db:abort_transaction({error, {changeset, Changeset}})
            end
        end, Changesets),
        {ok, DataList}
    end,
    case ebank_db:with_transaction(Insert) of
        {ok, DataList} ->
            {ok, DataList};
        {error, Reason} ->
            {error, Reason}
    end.

do_update(Changesets, Schema) when is_list(Changesets) ->
    Table = ebank_schema:table(Schema),
    Update = fun() ->
        DataList = lists:map(fun(Changeset) ->
            case changeset:is_valid(Changeset) of
                true ->
                    Changes = changeset:get_changes(Changeset),
                    Data = changeset:get_data(Changeset),
                    NewData = maps:merge(Data, Changes),
                    Record = ebank_schema:to_record(NewData, Schema),
                    case ebank_db:write(Record, Table) of
                        ok ->
                            Record;
                        {error, Reason} ->
                            ebank_db:abort_transaction({error, Reason})
                    end;
                false ->
                    ebank_db:abort_transaction({error, {changeset, Changeset}})
            end
        end, Changesets),
        {ok, DataList}
    end,
    case ebank_db:with_transaction(Update) of
        {ok, DataList} ->
            {ok, DataList};
        {error, Reason} ->
            {error, Reason}
    end.

do_normalize_data(Data, Schema) ->
    Normalizations = [
        {redacted, ebank_schema:redacted_fields(Schema)}
    ],
    do_normalize_data(Normalizations, Data, Schema).

do_normalize_data([{redacted, []} | T], Data, Schema) ->
    do_normalize_data(T, Data, Schema);
do_normalize_data([{redacted, Redacted} | T], Data0, Schema) ->
    Data = replace_data(Redacted, Data0, <<"***">>, Schema),
    do_normalize_data(T, Data, Schema);
do_normalize_data([], Data, _Schema) ->
    Data.

replace_data(Fields, Data, Replacement, Schema) ->
    lists:foldl(fun(Field, Acc) ->
        ebank_schema:set_field_value(Field, Replacement, Acc, Schema)
    end, Data, Fields).
