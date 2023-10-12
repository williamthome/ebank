-module(ebank_dsl_adapter_mnesia).

-behaviour(ebank_dsl_adapter).

%% API functions
-export([ query/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

query(Clauses, Indexes) ->
    ebank_qlc:compile(ebank_qlc:query(mnesia, Clauses, Indexes)).
