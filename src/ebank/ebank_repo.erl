-module(ebank_repo).

%% API functions
-export([ create_table/2
        , insert_one/2
        , fetch/3
        , fetch_one/3
        , update_one/3
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

fetch(Query, Bindings, SchemaMod) ->
    Fetch = fun() -> ebank_db:read(Query, Bindings) end,
    case ebank_db:with_transaction(Fetch) of
        {ok, DataList} ->
            Schema = SchemaMod:schema(),
            {ok, normalize_data_list(DataList, Schema)};
        {error, Reason} ->
            {error, Reason}
    end.

fetch_one(Query, Bindings, SchemaMod) ->
    normalize_one_data_result(fetch(Query, Bindings, SchemaMod)).

update_one(Record, Params, SchemaMod) ->
    Schema = SchemaMod:schema(),
    IndexesName = ebank_maps:invert(ebank_schema:fields_index(Schema)),
    Data = ebank_records:to_map(IndexesName, Record),
    Changeset = SchemaMod:changeset(Data, Params),
    normalize_one_data_result(do_update([Changeset], Schema)).

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
        {ok, normalize_data_list(DataList, Schema)}
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
        {ok, normalize_data_list(DataList, Schema)}
    end,
    case ebank_db:with_transaction(Update) of
        {ok, DataList} ->
            {ok, DataList};
        {error, Reason} ->
            {error, Reason}
    end.

normalize_data_list(DataList, Schema) ->
    case ebank_schema:redacted_fields(Schema) of
        [] ->
            DataList;
        Redacted ->
            lists:map(fun(Data) ->
                replace_data(Redacted, Data, <<"***">>, Schema)
            end, DataList)
    end.

replace_data(Fields, Data, Replacement, Schema) ->
    lists:foldl(fun(Field, Acc) ->
        ebank_schema:set_field_value(Field, Replacement, Acc, Schema)
    end, Data, Fields).
