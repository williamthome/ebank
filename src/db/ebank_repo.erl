-module(ebank_repo).

%% API functions
-export([ create_tables/0
        , create_table/1
        , insert/2
        ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

create_tables() ->
    Models = ebank_env:get(models),
    Schemas = lists:map(fun(Mod) -> Mod:schema() end, Models),
    do_create_tables(Schemas).

do_create_tables([Schema | Schemas]) ->
    case create_table(Schema) of
        ok ->
            do_create_tables(Schemas);
        {error, Reason} ->
            {error, Reason}
    end;
do_create_tables([]) ->
    ok.

create_table(Schema) ->
    ebank_db:create_table(#{
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
