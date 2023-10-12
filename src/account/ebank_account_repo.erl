-module(ebank_account_repo).

%% API functions
-export([ insert/1, fetch/1, update/2 ]).

%% Libs
-include("ebank_repo.hrl").

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

insert(Params) ->
    ebank_repo:insert_one(Params, ebank_account:schema()).

fetch(Id) ->
    Schema = ebank_account:schema(),
    Table = ebank_schema:table(Schema),
    FieldsIndex = ebank_schema:fields_index(Schema),
    Indexes = #{Table => FieldsIndex},
    Clauses = [{{Table, id}, '=:=', Id}],
    Query = ebank_dsl:query(Clauses, Indexes),
    ebank_repo:fetch_one(Query).

update(Id, Params) ->
    case fetch(Id) of
        {ok, Record} ->
            ebank_repo:update_one(Record, Params, ebank_account:schema());
        {error, Reason} ->
            {error, Reason}
    end.
