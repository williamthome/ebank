-module(ebank_db_adapter_mnesia).

-feature(maybe_expr, enable).

-behaviour(ebank_db_adapter).

%% ebank_db callbacks
-export([ connect/1
        , create_table/1
        , with_transaction/1
        , insert/2
        , fetch/2
        ]).

%% API functions
-export([ start_mnesia/0 ]).

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
    Name = maps:get(name, Args),
    Record = maps:get(record, Args, Name),
    Fields = maps:get(fields, Args),
    Indexes = maps:get(indexes, Args, []),
    Nodes = maps:get(nodes, Args, [node()]),
    Persist = maps:get(persist, Args, false),
    do_create_table(Name, Record, Fields, Indexes, Nodes, Persist).

with_transaction(Fun) ->
    normalize_result(mnesia:transaction(Fun)).

insert(Data, Table) ->
    normalize_result(mnesia:write(Table, Data, write)).

fetch(Clauses, Indexes) ->
    Query = query_comprehension(Clauses, Indexes),
    {ok, eval_query(Query)}.

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
                {protection, private},
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

query_comprehension(Clauses, Indexes) ->
    query_comprehension(mnesia, Clauses, Indexes).

query_comprehension(DB, Clauses, Indexes) ->
    [$[, query_body(DB, Clauses, Indexes), $]].

query_body(DB, Clauses, Indexes) ->
    QTables = query_tables(DB, maps:keys(Indexes)),
    QClauses = query_clauses(Clauses, Indexes, []),
    lists:join($,, lists:reverse(lists:merge(QTables, QClauses))).

query_tables(DB, Tables) ->
    lists:map(fun(Table) -> query_table(DB, Table) end, Tables).

query_table(DB, Table) ->
    Alias = table_alias(Table),
    io_lib:format("~s || ~s <- ~w:table(~w)", [Alias, Alias, DB, Table]).

query_clauses([{'orelse', List} | T], Indexes, Acc) ->
    Query = [$(, lists:join(" orelse ", query_clauses(List, Indexes, [])), $)],
    query_clauses(T, Indexes, [Query | Acc]);
query_clauses([{'andalso', List} | T], Indexes, Acc) ->
    Query = [$(, lists:join(" andalso ", query_clauses(List, Indexes, [])), $)],
    query_clauses(T, Indexes, [Query | Acc]);
query_clauses([{{Table, Field}, Op, Val} | T], Indexes, Acc) ->
    % @note: plus one due to the record name: {record_name, ...attrs}
    Index = maps:get(Field, maps:get(Table, Indexes)) + 1,
    TableAlias = table_alias(Table),
    Query = io_lib:format("element(~p, ~s) ~s ~p", [Index, TableAlias, atom_to_list(Op), Val]),
    query_clauses(T, Indexes, [Query | Acc]);
query_clauses([], _, Acc) ->
    lists:reverse(Acc).

table_alias(Table) ->
    do_table_alias(atom_to_binary(Table)).

do_table_alias(<<H, T/binary>>) ->
    <<(H - 32), T/binary>>.

% @todo: use parse transformation to compile queries and boost performance.
eval_query(Query) ->
    {ok, Tokens, _} = erl_scan:string(lists:flatten([Query, $.])),
    {ok, [Expr]} = erl_parse:parse_exprs(Tokens),
    {ok, QueryExpr} = qlc_pt:transform_expression(Expr, []),
    {value, QH, []} = erl_eval:expr(QueryExpr, []),
    qlc:eval(QH).

%%----------------------------------------------------------------------
%% TESTS
%%----------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

normalize_clause_test() ->
    Indexes = #{user => #{name => 1, age => 2}},
    Clauses = [
        {'orelse', [
            {'andalso', [
                {{user, name}, '=:=', <<"Joe">>},
                {{user, age}, '>=', 68}
            ]},
            {{user, name}, '=:=', <<"Mike">>},
            {'orelse', [
                {{user, name}, '=:=', <<"Robert">>},
                {{user, name}, '=:=', <<"Virding">>}
            ]}
        ]}
    ],
    Expected =
        "[User || User <- mnesia:table(user),("
        "(element(2, User) =:= <<\"Joe\">> andalso element(3, User) >= 68) "
        "orelse element(2, User) =:= <<\"Mike\">> "
        "orelse (element(2, User) =:= <<\"Robert\">> orelse element(2, User) =:= <<\"Virding\">>)"
        ")]",
    ?assertEqual(Expected, lists:flatten(query_comprehension(Clauses, Indexes))).

eval_query_test() ->
    ets:new(user, [bag, named_table]),
    ets:insert(user, {user, <<"Joe">>, 68}),
    ets:insert(user, {user, <<"Guest">>, undefined}),
    ets:insert(user, {user, <<"Mike">>, undefined}),
    ets:insert(user, {user, <<"Robert">>, undefined}),
    ets:insert(user, {user, <<"Anonymous">>, 0}),
    ets:insert(user, {user, <<"Anyone">>, 10}),

    DB = ets,
    Indexes = #{user => #{name => 1, age => 2}},
    Clauses = [
        {'orelse', [
            {'andalso', [
                {{user, name}, '=:=', <<"Joe">>},
                {{user, age}, '>=', 68}
            ]},
            {{user, name}, '=:=', <<"Mike">>},
            {'orelse', [
                {{user, name}, '=:=', <<"Robert">>},
                {{user, name}, '=:=', <<"Virding">>}
            ]},
            {{user, age}, '=:=', 10}
        ]}
    ],

    Expected = [
        {user,<<"Joe">>,68},
        {user,<<"Mike">>,undefined},
        {user,<<"Robert">>,undefined},
        {user,<<"Anyone">>,10}
    ],
    ?assertEqual(Expected, eval_query(query_comprehension(DB, Clauses, Indexes))).

-endif.
