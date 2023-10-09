-module(ebank_db_mnesia).

-feature(maybe_expr, enable).

-behaviour(ebank_db).

%% ebank_db callbacks
-export([ connect/1
        , create_table/1
        , with_transaction/1
        , insert/2
        ]).

%%----------------------------------------------------------------------
%% EBANK_DB CALLBACKS
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
    mnesia:create_table(maps:get(name, Args), [
        {type, set},
        {attributes, maps:get(fields_name, Args)},
        {index, maps:get(indexes, Args, [])},
        {disc_copies, maps:get(nodes, Args, [node()])},
        {storage_properties, [
            {ets, [
                private,
                named_table,
                {write_concurrency, true},
                {read_concurrency, true},
                {decentralized_counters, true}
            ]} |
            case maps:get(persist, Args, true) of
                true ->
                    [{dets, [
                        {auto_save, maps:get(persist_interval, Args)}
                    ]}];
                false ->
                    []
            end
        ]}
    ]).

% @todo: normalize result.
with_transaction(Fun) ->
    mnesia:transaction(Fun).

insert(Data, Table) ->
    mnesia:write(Table, Data, write).

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
    case rpc:multicall(Nodes, application, start, [mnesia]) of
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
