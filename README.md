# ebank

A small financial OTP application for personal studies.

## Goals

Be reliable, have no bugs, clean code, be scalable, and use the minor third-party libraries as possible. The idea to use fewer third-party libs is not to `reinvent the wheel`, but to do most of the code by myself. To achieve these goals I do not doubt that Erlang or Elixir are the right choice, so I'm going with Erlang.

## Project structure

The project is structured to have domain and business logic separated from what it exposes to the world, also has adapters to plug third-party libraries and not have a direct dependency on them.
I really enjoy how the [Elixir](https://elixir-lang.org/) code is structured, so some concepts and directory structures are based on Elixir libraries or applications.

## Directory structure

```
.
|-- src: the source folder, contains the main code.
    |-- ebank: holds the business and domain code.
    |   |-- account: the account entity.
    |   |-- db: database adapters.
    |   |-- dsl: query adapters.
    |   |-- json: JSON parser adapters.
    |   |-- model: behavior that interacts with the database.
    |   |-- schema: behavior that holds the database schemas.
    |   |-- server: server adapters.
    |-- ebank_web: expose the ebank directory to the world.
        |-- controller: resolves HTTP requests.
```

## Data validation

This application uses `changeset`, an Erlang library created and maintained by me. It provides an interface based on the [Elixir/Ecto changeset](https://hexdocs.pm/ecto/Ecto.Changeset.html) library.
A changeset is required to manipulate any data in the database, in this way, any data is validated before persists.

## Database

The current database used is [mnesia](https://www.erlang.org/doc/man/mnesia.html). Mnesia it's a built Erlang database that provides:

> - A relational/object hybrid data model that is suitable for telecommunications applications.
> - A DBMS query language, Query List Comprehension (QLC) as an add-on library.
> - Persistence. Tables can be coherently kept on disc and in the main memory.
> - Replication. Tables can be replicated at several nodes.
> Atomic transactions. A series of table manipulation operations can be grouped into a single atomic transaction.
> - Location transparency. Programs can be written without knowledge of the actual data location.
> - Extremely fast real-time data searches.
> - Schema manipulation routines. The DBMS can be reconfigured at runtime without stopping the system.

Mnesia works really well, it's reliable, scalable, and fits great to this simple project.

## Query

The query system is provided via `DSL` (Domain Specific Language), where Erlang code is compiled into the database syntax. The syntax expects a list of tuples with this format: `{{Table, Field}, Operator, Value}`.

- The `Table` and the `Field` are atoms defined in the schema (see [metaprogramming](#metaprogramming]));
- The `Operator` is any [Erlang operators](https://www.erlang.org/doc/reference_manual/expressions.html#term-comparisons), like `=:=`, `=/=`, `>`, `<`, etc;
- The `Value` is any Erlang term or, if it is an atom starting with `@`, it will be considered as a variable that can be defined at the runtime, e.g. `@foo`.

They can be linked using `orelse` and `andalso`, and to resolve the variables a map with the index of each field of each table evolved in the query is required, e.g. `#{table_a => #{foo => 1}, table_b => #{bar => 1, baz => 2}}`, e.g.:


```erlang
Table = mytable,
Indexes = #{mytable => #{foo => 1, bar => 2, baz => 3}},
Clauses=[{'andalso', [
    {{Table, foo}, '=:=', <<"bar">>}, % <- static
    {'orelse', [
        {{Table, bar}, '=/=', '@baz'}, % <- variable 'baz'
        {{Table, baz}, '>=', 0} % <- static
    ]}
]}],
Query = ebank_dsl:query(Clauses, Indexes),
ebank_db:read(Query, #{baz => the_baz_value}).
```

This project uses [qlc](https://www.erlang.org/doc/man/qlc) to resolve `mnesia` queries, so the output is a [list comprehension](https://www.erlang.org/docs/23/programming_examples/list_comprehensions.html):

```erlang
[ Mytable || Mytable <- mnesia:table(mytable)
, ( element(2, Mytable) =:= <<"bar">>
    andalso ( element(3, Mytable) =/= maps:get(baz, Bindings)
              orelse element(4, Mytable) >= 0 )
  )
]
```

To illustrate, the result of the query above in `PostgreSQL` should be:

```sql
WHERE mytable.foo = 'bar'
  AND ( mytable.bar != $1 OR mytable.baz >= 0 )
```

## Metaprogramming

This project use `parse_transform` to do [metaprogramming](https://www.erlang-factory.com/static/upload/media/1434462166791692seancribbseuc2015.pdf).
The reason behind that is to simplify the code and make it more scalable, readable, and easy to maintain.

Currently, the project contains two modules that provide metaprogramming:

- `ebank_model_transform`: used by the database models. The module that uses it should expose a `-model` attribute. Currently, it compiles the queries to improve performance. Example:
    ```erlang
    -model(#{
        schema => ebank_account_schema,
        queries => [ q_fetch_by_id/0, q_exists/0 ]
    }).
    ```
- `ebank_schema_transform`: used by database schemas. The module that uses it should expose a `-schema` attribute. Currently, it compiles the schema and exposes it on a `schema/0` function in the module. Out of the box, it provides type definitions and validations via changesets for the fields. Example:
    ```erlang
    -schema(#{
        table => account,
        fields => [
            {id, {integer, [readonly]}},
            {social_id, {binary, [required, indexed]}},
            {name, {binary, [required]}},
            {password, {binary, [required, redacted]}},
            {created_at, {datetime, [readonly]}}
        ]
    }).
    ```

_NOTE: An Erlang helper library called [parserl](https://github.com/williamthome/parserl) is used to simplify the metaprogramming. I'm the author and it is maintained by me._

## Third-party Deps

- [Cowboy](https://github.com/ninenines/cowboy): HTTP server;
- [Thoas](https://github.com/lpil/thoas): JSON parser.

## What it offers

The `make prod` command in the root folder starts the application in production mode, `make dev` to start in developer mode, or `make daemon` to start in developer mode and start a hot code reloading.
The command starts a server at the `8080` port and these routes are exposed:

- `[POST] /accounts`: create an account if all parameters are valid and only if no one with the desired `social_id` exists;
- `[GET] /accounts/:id`: return an account if some exists with the informed `id`;
- `[PATCH] /accounts/:id`: update an account if some exists with the informed `id` (TODO: allow updates only for authorized/logged user(s));

## Disclaimer

This is an under-development application. Any change can occur without any notice. There are a lot of things to do and improve.

## TODOs:

- [ ] Create a table called `transactions` where accounts can share money;
- [ ] Create a table called `tokens` or `sessions` where the user can log in and perform updates and transactions;
