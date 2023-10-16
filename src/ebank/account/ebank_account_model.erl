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
    case do_fetch_by_id(Id) of
        {ok, Account} ->
            {ok, ebank_repo:normalize_data(Account, ?SCHEMA_MOD)};
        {error, Reason} ->
            {error, Reason}
    end.

update(Id, Params) ->
    case do_fetch_by_id(Id) of
        {ok, Account} ->
            case ebank_repo:update_one(Account, Params, ?SCHEMA_MOD) of
                {ok, UpdatedAccount} ->
                    {ok, ebank_repo:normalize_data(UpdatedAccount, ?SCHEMA_MOD)};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%----------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%----------------------------------------------------------------------

do_fetch_by_id(Id) ->
    Query = q_fetch_by_id(),
    Bindings = #{id => Id},
    ebank_repo:fetch_one(Query, Bindings, ?SCHEMA_MOD).

q_fetch_by_id() ->
    Schema = ?SCHEMA_MOD:schema(),
    Table = ebank_schema:table(Schema),
    FieldsIndex = ebank_schema:fields_index(Schema),
    Indexes = #{Table => FieldsIndex},
    Clauses = [{{Table, id}, '=:=', '@id'}],
    ebank_dsl:query(Clauses, Indexes).
