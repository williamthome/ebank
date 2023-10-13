-module(ebank_account_model).

%% API functions
-export([ insert/1, fetch/1, update/2 ]).

%% Libs
-include("ebank_model.hrl").

%% Macros
-define(SCHEMA, ebank_account_schema).

%% Schema
-schema(?SCHEMA).
-query([ q_fetch_by_id/0 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

insert(Params) ->
    ebank_repo:insert_one(Params, ?SCHEMA).

fetch(Id) ->
    ebank_repo:fetch_one(q_fetch_by_id(), #{
        id => Id
    }).

update(Id, Params) ->
    case fetch(Id) of
        {ok, Record} ->
            ebank_repo:update_one(Record, Params, ?SCHEMA);
        {error, Reason} ->
            {error, Reason}
    end.

%%----------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%----------------------------------------------------------------------

q_fetch_by_id() ->
    Schema = ?SCHEMA:schema(),
    Table = ebank_schema:table(Schema),
    FieldsIndex = ebank_schema:fields_index(Schema),
    Indexes = #{Table => FieldsIndex},
    Clauses = [{{Table, id}, '=:=', '@id'}],
    ebank_dsl:query(Clauses, Indexes).
