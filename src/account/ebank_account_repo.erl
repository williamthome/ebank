-module(ebank_account_repo).

%% API functions
-export([ insert/1, get_by_id/1 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

insert(Params) ->
    Changeset = ebank_account:changeset(#{}, Params),
    ebank_repo:insert(Changeset, ebank_account:schema()).

get_by_id(Id) ->
    Table = ebank_schema:table(ebank_account:schema()),
    Clauses = [{{Table, id}, '=:=', Id}],
    case ebank_repo:fetch(Clauses, ebank_account:schema()) of
        {ok, [Account]} ->
            {ok, Account};
        {ok, []} ->
            {error, enoent};
        {error, Reason} ->
            {error, Reason}
    end.
