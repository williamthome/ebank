-module(ebank_account_model).

%% API functions
-export([ insert/1, exists/1, exists/2, fetch/1, update/2 ]).

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
    case ebank_repo:insert_one(Params, ?SCHEMA_MOD) of
        {ok, Account} ->
            {ok, ebank_repo:normalize_data(Account, ?SCHEMA_MOD)};
        {error, Reason} ->
            {error, Reason}
    end.

exists(SocialId) ->
    exists(undefined, SocialId).

exists(Id, SocialId) ->
    Query = q_exists(),
    Bindings = #{id => Id, social_id => SocialId},
    case ebank_repo:fetch(Query, Bindings, ?SCHEMA_MOD) of
        {ok, []} ->
            false;
        {ok, Accounts} ->
            {true, Accounts};
        {error, _} ->
            false
    end.

fetch(Id) ->
    case do_fetch_by_id(Id) of
        {ok, Account} ->
            {ok, ebank_repo:normalize_data(Account, ?SCHEMA_MOD)};
        {error, Reason} ->
            {error, Reason}
    end.

update(Id, Params) ->
    case do_fetch_by_id(Id) of
        {ok, Data} ->
            case ebank_repo:update_one(Data, Params, ?SCHEMA_MOD) of
                {ok, Account} ->
                    {ok, ebank_repo:normalize_data(Account, ?SCHEMA_MOD)};
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

q_exists() ->
    Schema = ?SCHEMA_MOD:schema(),
    Table = ebank_schema:table(Schema),
    FieldsIndex = ebank_schema:fields_index(Schema),
    Indexes = #{Table => FieldsIndex},
    Clauses = [{'orelse', [
        {{Table, id}, '=:=', '@id'},
        {{Table, social_id}, '=:=', '@social_id'}
    ]}],
    ebank_dsl:query(Clauses, Indexes).
