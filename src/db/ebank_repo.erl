-module(ebank_repo).

%% API functions
-export([ insert/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

insert(Changeset, Schema) ->
    case changeset:is_valid(Changeset) of
        true ->
            Changes = changeset:get_changes(Changeset),
            Record = ebank_schema:to_record(Changes, Schema),
            Table = ebank_schema:table(Schema),
            ebank_db:insert(Record, Table);
        false ->
            {error, {changeset, changeset:get_errors(Changeset)}}
    end.
