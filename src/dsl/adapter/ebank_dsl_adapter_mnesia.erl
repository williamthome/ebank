-module(ebank_dsl_adapter_mnesia).

-behaviour(ebank_dsl_adapter).

%% API functions
-export([ query/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

query(Clauses, Indexes) ->
    _Query = lists:flatten(ebank_qlc:query(mnesia, Clauses, Indexes)).
    % @todo: compile the query.
    % ebank_qlc:compile(Query, [{'Bindings', Bindings}]).
