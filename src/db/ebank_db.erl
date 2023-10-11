-module(ebank_db).

%% API functions
-export([ connect/0
        , create_table/1
        , with_transaction/1
        , insert/2
        , fetch/2
        ]).

-define(ADAPTER, (ebank_env:get_db(adapter))).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

connect() ->
    ?ADAPTER:connect(ebank_env:get_db(args)).

create_table(Args) ->
    ?ADAPTER:create_table(Args).

with_transaction(Fun) ->
    ?ADAPTER:with_transaction(Fun).

insert(Data, Table) ->
    ?ADAPTER:insert(Data, Table).

fetch(Clauses, Indexes) ->
    ?ADAPTER:fetch(Clauses, Indexes).
