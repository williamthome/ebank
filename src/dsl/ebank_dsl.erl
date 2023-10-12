-module(ebank_dsl).

%% API functions
-export([ query/2 ]).

%% Macros
-define(ADAPTER, (ebank_env:get_dsl(adapter))).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

query(Clauses, Indexes) ->
    ?ADAPTER:query(Clauses, Indexes).
