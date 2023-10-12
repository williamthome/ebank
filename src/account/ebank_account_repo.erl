-module(ebank_account_repo).

-behaviour(ebank_repo).

%% ebank_repo callbacks
-export([ model/0 ]).

%% API functions
-export([ insert/1, fetch/1, update/2 ]).

%% Libs
-include("ebank_repo.hrl").

%% Macros
-define(MODEL, ebank_account).

%% Queries
-query([ q_fetch_by_id/0 ]).

%%----------------------------------------------------------------------
%% EBANK_REPO CALLBACKS
%%----------------------------------------------------------------------

model() -> ?MODEL.

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

insert(Params) ->
    ebank_repo:insert_one(Params, ?MODEL).

fetch(Id) ->
    ebank_repo:fetch_one(q_fetch_by_id(), #{
        id => Id
    }).

update(Id, Params) ->
    case fetch(Id) of
        {ok, Record} ->
            ebank_repo:update_one(Record, Params, ?MODEL);
        {error, Reason} ->
            {error, Reason}
    end.

%%----------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%----------------------------------------------------------------------

q_fetch_by_id() ->
    Schema = ?MODEL:schema(),
    Table = ebank_schema:table(Schema),
    FieldsIndex = ebank_schema:fields_index(Schema),
    Indexes = #{Table => FieldsIndex},
    Clauses = [{{Table, id}, '=:=', '@id'}],
    ebank_dsl:query(Clauses, Indexes).
