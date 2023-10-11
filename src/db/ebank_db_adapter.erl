-module(ebank_db_adapter).

%% Types
-export_type([ data/0, table/0 ]).

-type transaction_fun() :: fun(() -> {ok, term()} | {error, term()}).
-type data() :: term().
-type table() :: atom().
-type field() :: atom().
-type indexes() :: #{table() => #{field() => pos_integer()}}.
-type clause() :: tuple().

%% Callbacks
-optional_callbacks([]).

-callback connect(Args) -> ok | {error, term()}
    when Args :: map().

-callback create_table(Args) -> ok | {error, term()}
    when Args :: map().

-callback with_transaction(Fun) -> ok | {error, term()}
    when Fun :: transaction_fun().

-callback abort_transaction(Reason) -> no_return()
    when Reason :: {error, term()}.

-callback read(Clauses, Indexes) -> ok | {error, term()}
    when Clauses :: [clause()]
       , Indexes :: indexes()
       .

-callback write(Data, Table) -> ok | {error, term()}
    when Data :: data()
       , Table :: table()
       .
