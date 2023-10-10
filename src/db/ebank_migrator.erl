-module(ebank_migrator).

%% API functions
-export([ migrate/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

% @todo: escript that executes migrations.
migrate(_Method = up, _Version = 1) ->
    EnvArgs = table_env_args(),
    Schemas = lists:map(fun(M) -> M:schema() end, ebank_env:get(models)),
    lists:foreach(fun(Schema) ->
        ebank_db:create_table(EnvArgs#{
            name => ebank_schema:table(Schema),
            record => ebank_schema:record(Schema),
            fields => ebank_schema:fields_name(Schema),
            indexes => ebank_schema:indexed_fields_name(Schema)
        })
    end, Schemas).

%%----------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%----------------------------------------------------------------------

% @todo: should be defined by schema.
table_env_args() ->
    case ebank_env:get_db(persist, false) of
        true ->
            #{ persist => true
             , persist_interval => ebank_env:get_db(persist_interval)
             };
        false ->
            #{persist => false}
    end.
