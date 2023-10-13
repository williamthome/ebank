-module(ebank_account_model).

%% API functions
-export([ insert/1, fetch/1, update/2 ]).

%% Libs
-include("ebank_model.hrl").

%% Macros
-define(SCHEMA_MOD, ebank_account_schema).

-model(#{
    schema => ?SCHEMA_MOD,
    queries => [ q_fetch_by_id/0 ]
}).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

insert(Params) ->
    ebank_repo:insert_one(Params, ?SCHEMA_MOD).

fetch(Id) ->
    Query = q_fetch_by_id(),
    Bindings = #{id => Id},
    ebank_repo:fetch_one(Query, Bindings, ?SCHEMA_MOD).

update(Id, Params) ->
    case fetch(Id) of
        {ok, Record} ->
            ebank_repo:update_one(Record, Params, ?SCHEMA_MOD);
        {error, Reason} ->
            {error, Reason}
    end.

%%----------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%----------------------------------------------------------------------

q_fetch_by_id() ->
    Schema = ?SCHEMA_MOD:schema(),
    Table = ebank_schema:table(Schema),
    FieldsIndex = ebank_schema:fields_index(Schema),
    Indexes = #{Table => FieldsIndex},
    Clauses = [{{Table, id}, '=:=', '@id'}],
    ebank_dsl:query(Clauses, Indexes).
