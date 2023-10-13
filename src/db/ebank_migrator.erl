-module(ebank_migrator).

%% API functions
-export([ migrate/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

% @todo: escript that executes migrations.
migrate(_Method = up, _Version = 1) ->
    Args = #{persist => ebank_env:get_db(persist, false)},
    Models = ebank_env:get(models),
    create_tables(Models, Args).

%%----------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%----------------------------------------------------------------------

create_tables([Model | Models], Args) ->
    case ebank_repo:create_table(Args, Model) of
        ok ->
            create_tables(Models, Args);
        {error, Reason} ->
            {error, Reason}
    end;
create_tables([], _) ->
    ok.
