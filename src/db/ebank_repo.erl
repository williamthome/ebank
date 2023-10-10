-module(ebank_repo).

%% API functions
-export([ insert/1 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

insert(Changeset) ->
    case changeset:is_valid(Changeset) of
        true ->
            Schema = changeset:get_metadata(Changeset),
            Changes = changeset:get_changes(Changeset),
            Record = ebank_schema:to_record(Changes, Schema),
            Table = ebank_schema:table(Schema),
            ebank_db:insert(Record, Table);
        false ->
            {error, {changeset, changeset:get_errors(Changeset)}}
    end.
