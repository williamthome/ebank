-module(ebank_db).

-behaviour(gen_server).

%% API functions
-export([ start_link/1
        , connect/1
        , create_table/1
        , with_transaction/1
        , insert/2
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_continue/2
        , handle_call/3
        , handle_cast/2
        ]).

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

%% gen_server state
-record(state, { adapter :: module()
               , args :: map()
               }).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

start_link(Args) ->
    Adapter = maps:get(adapter, Args),
    AdapterArgs = maps:get(args, Args),
    InitArgs = [Adapter, AdapterArgs],
    gen_server:start_link({local, ?MODULE}, ?MODULE, InitArgs, []).

connect(Args) ->
    gen_server:cast(?MODULE, {connect, Args}).

create_table(Args) ->
    gen_server:call(?MODULE, {create_table, Args}).

with_transaction(Fun) ->
    gen_server:call(?MODULE, {with_transaction, Fun}).

insert(Data, Table) ->
    gen_server:call(?MODULE, {insert, Data, Table}).

%%----------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%----------------------------------------------------------------------

init([Adapter, Args]) ->
    State = #state{ adapter = Adapter
                  , args = Args
                  },
    {ok, State, {continue, [Adapter, Args]}}.

% @todo: up tables.
handle_continue([Adapter, Args], State) ->
    case Adapter:connect(Args) of
        ok ->
            {noreply, State};
        {error, Reason} ->
            {stop, Reason, State}
    end.

handle_call({create_table, Args}, _From, State) ->
    Adapter = State#state.adapter,
    Reply = Adapter:create_table(Args),
    {reply, Reply, State};
handle_call({with_transaction, Fun}, _From, State) ->
    Adapter = State#state.adapter,
    Reply = Adapter:with_transaction(Fun),
    {reply, Reply, State};
handle_call({insert, Data, Table}, _From, State) ->
    Adapter = State#state.adapter,
    Reply = Adapter:insert(Data, Table),
    {reply, Reply, State}.

handle_cast({connect, Args}, State) ->
    Adapter = State#state.adapter,
    case Adapter:connect(Args) of
        ok ->
            {noreply, State};
        {error, Reason} ->
            {stop, Reason, State}
    end.
