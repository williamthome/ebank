-module(ebank_migrator).

%% API functions
-export([ migrate/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

% @todo: escript that executes migrations.
migrate(_Method = up, _Version = 1) ->
    Args = table_env_args(),
    Models = ebank_env:get(models),
    ebank_repo:create_tables(Args, Models).

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
