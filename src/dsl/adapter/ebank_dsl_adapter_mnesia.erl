-module(ebank_dsl_adapter_mnesia).

-behaviour(ebank_dsl_adapter).

%% API functions
-export([ query/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

query(Clauses, Indexes) ->
    _Query = lists:flatten(ebank_qlc:query(mnesia, Clauses, Indexes)).
    % @todo: find a mechanism to compile the query.
    %        The current implementation via parse_transform
    %        just compiles the string and not the qlc QH.
    %        @see: ebank_db_adapter_mnesia:read/2
    %        @see: https://www.erlang.org/doc/man/qlc
    % ebank_qlc:compile(Query, [{'Bindings', Bindings}]).
