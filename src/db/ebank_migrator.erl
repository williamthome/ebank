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
    ebank_repo:create_tables(Args, Models).
