-module(ebank_repo).

%% API functions
-export([ create_tables/2
        , create_table/2
        , insert_one/2
        , fetch/1
        , fetch_one/1
        , update_one/3
        ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

create_tables(Args, Models) ->
    Schemas = lists:map(fun(Mod) -> Mod:schema() end, Models),
    do_create_tables(Schemas, Args).

do_create_tables([Schema | Schemas], Args) ->
    case create_table(Args, Schema) of
        ok ->
            do_create_tables(Schemas, Args);
        {error, Reason} ->
            {error, Reason}
    end;
do_create_tables([], _) ->
    ok.

create_table(Args, Schema) ->
    ebank_db:create_table(Args#{
        name => ebank_schema:table(Schema),
        fields => ebank_schema:fields_name(Schema)
    }).

insert_one(Params, Schema) when is_map(Params) ->
    Changeset = ebank_schema:changeset(#{}, Params, Schema),
    normalize_one_data_result(do_insert([Changeset], Schema)).

fetch(Query) ->
    Fetch = fun() -> ebank_db:read(Query) end,
    case ebank_db:with_transaction(Fetch) of
        {ok, Data} ->
            {ok, Data};
        {error, Reason} ->
            {error, Reason}
    end.

fetch_one(Query) ->
    normalize_one_data_result(fetch(Query)).

update_one(Record, Params, Schema) ->
    FieldsIndex = ebank_schema:fields_index(Schema),
    IndexesName = maps:fold(fun(Name, Index, Acc) ->
        Acc#{Index => Name}
    end, #{}, FieldsIndex),
    Data = ebank_records:to_map(IndexesName, Record),
    Changeset = ebank_schema:changeset(Data, Params, Schema),
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
        {ok, lists:map(fun(Changeset) ->
            case changeset:is_valid(Changeset) of
                true ->
                    Changes = changeset:get_changes(Changeset),
                    Record = ebank_schema:to_record(Changes, Schema),
                    case ebank_db:write(Record, Table) of
                        ok ->
                            Changeset;
                        {error, Reason} ->
                            ebank_db:abort_transaction({error, Reason})
                    end;
                false ->
                    ebank_db:abort_transaction({error, {changeset, Changeset}})
            end
        end, Changesets)}
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
        {ok, lists:map(fun(Changeset) ->
            case changeset:is_valid(Changeset) of
                true ->
                    Changes = changeset:get_changes(Changeset),
                    Data = changeset:get_data(Changeset),
                    NewData = maps:merge(Data, Changes),
                    Record = ebank_schema:to_record(NewData, Schema),
                    case ebank_db:write(Record, Table) of
                        ok ->
                            Changeset;
                        {error, Reason} ->
                            ebank_db:abort_transaction({error, Reason})
                    end;
                false ->
                    ebank_db:abort_transaction({error, {changeset, Changeset}})
            end
        end, Changesets)}
    end,
    case ebank_db:with_transaction(Update) of
        {ok, DataList} ->
            {ok, DataList};
        {error, Reason} ->
            {error, Reason}
    end.
