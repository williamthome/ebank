-module(ebank_repo).

%% API functions
-export([ create_tables/2
        , create_table/2
        , insert/2
        , fetch/2
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

insert(Changeset, Schema) ->
    case changeset:is_valid(Changeset) of
        true ->
            Changes = changeset:get_changes(Changeset),
            Data = ebank_schema:to_record(Changes, Schema),
            Table = ebank_schema:table(Schema),
            Insert = fun() -> ebank_db:insert(Data, Table) end,
            case ebank_db:with_transaction(Insert) of
                ok ->
                    {ok, Changeset};
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            {error, {changeset, Changeset}}
    end.

fetch(Clause, Schema) ->
    Table = ebank_schema:table(Schema),
    FieldsIndexes = ebank_schema:fields_index(Schema),
    Indexes = #{Table => FieldsIndexes},
    Get = fun() -> ebank_db:fetch(Clause, Indexes) end,
    case ebank_db:with_transaction(Get) of
        {ok, Data} ->
            {ok, Data};
        {error, Reason} ->
            {error, Reason}
    end.
