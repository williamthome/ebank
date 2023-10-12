-module(ebank_qlc).

%% API functions
-export([ query/3, compile/2, eval/1 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

query(DB, Clauses, Indexes) ->
    [$[, query_body(DB, Clauses, Indexes), $]].

compile(Query, Bindings) ->
    qlc:string_to_handle(lists:flatten([Query, $.]), [], Bindings).

eval(Compiled) ->
    qlc:eval(Compiled).

%%----------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%----------------------------------------------------------------------

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
query_clauses([{{Table, Field}, Op, Atom} | T], Indexes, Acc) when is_atom(Atom) ->
    case atom_to_binary(Atom) of
        <<$@, Var/binary>> ->
            Index = field_index(Field, Table, Indexes),
            TableAlias = table_alias(Table),
            Query = io_lib:format( "element(~p, ~s) ~s maps:get(~s, Bindings)"
                                , [Index, TableAlias, atom_to_list(Op), Var] ),
            query_clauses(T, Indexes, [Query | Acc]);
        _ ->
            Index = field_index(Field, Table, Indexes),
            TableAlias = table_alias(Table),
            Query = io_lib:format( "element(~p, ~s) ~s ~p"
                                , [Index, TableAlias, atom_to_list(Op), Atom] ),
            query_clauses(T, Indexes, [Query | Acc])
    end;
query_clauses([{{Table, Field}, Op, Val} | T], Indexes, Acc) ->
    Index = field_index(Field, Table, Indexes),
    TableAlias = table_alias(Table),
    Query = io_lib:format( "element(~p, ~s) ~s ~p"
                         , [Index, TableAlias, atom_to_list(Op), Val] ),
    query_clauses(T, Indexes, [Query | Acc]);
query_clauses([], _, Acc) ->
    lists:reverse(Acc).

% @note: plus one due to the record name: {record_name, ...attrs}
field_index(Field, Table, Indexes) ->
    maps:get(Field, maps:get(Table, Indexes)) + 1.

table_alias(Table) ->
    do_table_alias(atom_to_binary(Table)).

do_table_alias(<<H, T/binary>>) ->
    <<(H - 32), T/binary>>.

%%----------------------------------------------------------------------
%% TESTS
%%----------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

normalize_clause_test() ->
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
            ]}
        ]}
    ],
    Expected =
        "[User || User <- ets:table(user),("
        "(element(2, User) =:= <<\"Joe\">> andalso element(3, User) >= 68) "
        "orelse element(2, User) =:= <<\"Mike\">> "
        "orelse (element(2, User) =:= <<\"Robert\">> orelse element(2, User) =:= <<\"Virding\">>)"
        ")]",
    ?assertEqual(Expected, lists:flatten(query(DB, Clauses, Indexes))).

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
    ?assertEqual(Expected, eval(query(DB, Clauses, Indexes))).

-endif.
