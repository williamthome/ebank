-module(ebank_migrator).

%% API functions
-export([ migrate/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

% @todo: escript that executes migrations.
migrate(_Method = up, _Version = 1) ->
    Args = #{persist => ebank_env:get_db(persist, false)},
    Schemas = ebank_env:get(schemas),
    create_tables(Schemas, Args).

%%----------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%----------------------------------------------------------------------

create_tables([Schema | Schemas], Args) ->
    case ebank_repo:create_table(Args, Schema) of
        ok ->
            create_tables(Schemas, Args);
        {error, Reason} ->
            {error, Reason}
    end;
create_tables([], _) ->
    ok.
