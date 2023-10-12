-module(ebank_db).

%% API functions
-export([ connect/0
        , create_table/1
        , with_transaction/1
        , abort_transaction/1
        , read/1
        , write/2
        ]).

%% Macros
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

abort_transaction(Reason) ->
    ?ADAPTER:abort_transaction(Reason).

read(Query) ->
    ?ADAPTER:read(Query).

write(Data, Table) ->
    ?ADAPTER:write(Data, Table).
