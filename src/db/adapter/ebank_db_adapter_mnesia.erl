-module(ebank_db_adapter_mnesia).

% @note: from OTP-27 this flag can be removed.
-feature(maybe_expr, enable).

-behaviour(ebank_db_adapter).

%% ebank_db_adapter callbacks
-export([ connect/1
        , create_table/1
        , with_transaction/1
        , abort_transaction/1
        , read/2
        , write/2
        ]).

%% API functions
-export([ start_mnesia/0 ]).

%%----------------------------------------------------------------------
%% EBANK_DB_ADAPTER CALLBACKS
%%----------------------------------------------------------------------

connect(Args) ->
    Dir = maps:get(dir, Args),
    Nodes = maps:get(nodes, Args, [node()]),
    maybe
        ok ?= set_dir(Dir),
        ok ?= create_schemas(Nodes),
        ok ?= start(Nodes),
        ok
    end.

create_table(Args) ->
    Name = maps:get(name, Args),
    Record = maps:get(record, Args, Name),
    Fields = maps:get(fields, Args),
    Indexes = maps:get(indexes, Args, []),
    Nodes = maps:get(nodes, Args, [node()]),
    Persist = maps:get(persist, Args, false),
    do_create_table(Name, Record, Fields, Indexes, Nodes, Persist).

with_transaction(Fun) ->
    normalize_result(mnesia:transaction(Fun)).

abort_transaction(Reason) ->
    mnesia:abort(Reason).

% @todo: compile query via parse transform.
read(Query, Bindings) ->
    {ok, ebank_qlc:eval(ebank_qlc:compile(Query, [{'Bindings', Bindings}]))}.

write(Data, Table) ->
    normalize_result(mnesia:write(Table, Data, write)).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

start_mnesia() ->
    case application:start(mnesia) of
        ok ->
            ok;
        {error, {already_started, mnesia}} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%----------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%----------------------------------------------------------------------

set_dir(Dir) ->
    application:set_env(mnesia, dir, Dir).

create_schemas(Nodes) ->
    case mnesia:create_schema(Nodes) of
        ok ->
            ok;
        {error, {_, {already_exists, _}}} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

start(Nodes) ->
    case rpc:multicall(Nodes, ?MODULE, start_mnesia, []) of
        {Results, []} ->
            case lists:filter(fun(Result) -> Result =/= ok end, Results) of
                [] ->
                    ok;
                Errors ->
                    {error, {mnesia, Errors}}
            end;
        {_, BadNodes} ->
            {error, {mnesia, {bad_nodes, BadNodes}}}
    end.

do_create_table(Name, Record, Fields, Indexes, Nodes, Persist) ->
    Result = do_create_table_1(Name, Record, Fields, Indexes, Nodes, Persist),
    case normalize_result(Result) of
        ok ->
            ok;
        {error, {already_exists, Name}} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

do_create_table_1(Name, Record, Fields, Indexes, Nodes, Persist) ->
    mnesia:create_table(Name, [
        {type, set},
        {record_name, Record},
        {attributes, Fields},
        {index, Indexes},
        {disc_copies, Nodes},
        {storage_properties, [
            {ets, [
                % @todo: protect table against external changes.
                % @see: https://www.erlang.org/doc/man/ets#private
                % {protection, private},
                {write_concurrency, true},
                {read_concurrency, true},
                {decentralized_counters, true}
            ]} |
            case Persist of
                {true, PersistInterval} ->
                    [{dets, [
                        {auto_save, PersistInterval}
                    ]}];
                false ->
                    []
            end
        ]}
    ]).

normalize_result({atomic, ok}) ->
    ok;
normalize_result({atomic, {ok, Result}}) ->
    {ok, Result};
normalize_result({atomic, Result}) ->
    {ok, Result};
normalize_result({aborted, {error, Reason}}) ->
    {error, Reason};
normalize_result({aborted, Reason}) ->
    {error, Reason};
normalize_result(ok) ->
    ok;
normalize_result({ok, Result}) ->
    {ok, Result};
normalize_result({error, Reason}) ->
    {error, Reason}.
