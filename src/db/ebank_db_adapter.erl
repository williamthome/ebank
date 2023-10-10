-module(ebank_db_adapter).

%% Types
-export_type([ data/0, table/0 ]).

-type data() :: term().
-type table() :: atom().
-type transaction_fun() :: fun(() -> {ok, term()} | {error, term()}).

%% Callbacks
-optional_callbacks([]).

-callback connect(Args) -> ok | {error, term()}
    when Args :: map().

-callback insert(Data, Table) -> ok | {error, term()}
    when Data :: data()
       , Table :: table().

-callback with_transaction(Fun) -> {ok, term()} | {error, term()}
    when Fun :: transaction_fun().
