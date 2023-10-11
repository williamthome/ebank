-module(ebank_account_repo).

%% API functions
-export([ insert/1, fetch/1, update/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

insert(Params) ->
    ebank_repo:insert_one(Params, ebank_account:schema()).

fetch(Id) ->
    Table = ebank_schema:table(ebank_account:schema()),
    Clauses = [{{Table, id}, '=:=', Id}],
    ebank_repo:fetch_one(Clauses, ebank_account:schema()).

update(Id, Params) ->
    case fetch(Id) of
        {ok, Record} ->
            ebank_repo:update_one(Record, Params, ebank_account:schema());
        {error, Reason} ->
            {error, Reason}
    end.
