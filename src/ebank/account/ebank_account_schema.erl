-module(ebank_account_schema).

-behaviour(ebank_schema).

%% ebank_schema callbacks
-export([ changeset/2 ]).

%% API functions
-export([ to_map/1 ]).

%% Libs
-include("ebank_schema.hrl").

-schema(#{
    table => account,
    fields => [
        {id, {integer, [readonly]}},
        {social_id, {binary, [required, indexed]}},
        {name, {binary, [required]}},
        {password, {binary, [required, redacted]}},
        {created_at, {datetime, [readonly]}}
    ]
}).

%%----------------------------------------------------------------------
%% EBANK_SCHEMA CALLBACKS
%%----------------------------------------------------------------------

changeset(Data, Params) ->
    Changeset = ebank_schema:changeset(Data, Params, ?MODULE:schema()),
    case changeset:is_valid(Changeset) of
        true ->
            changeset:pipe(Changeset, [
                fun check_unique/1,
                fun set_id/1,
                fun hash_password/1,
                fun set_created_at/1
            ]);
        false ->
            Changeset
    end.

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

to_map(Account) ->
    ebank_schema:to_map(Account, ?MODULE:schema()).

%%----------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%----------------------------------------------------------------------

check_unique(Changeset) ->
    case changeset:find_change(social_id, Changeset) of
        {ok, SocialId} ->
            case ebank_account_model:exists(SocialId) of
                {true, _Account} ->
                    changeset:push_error({social_id,
                        {<<"account with social_id already exists">>, #{
                            validation => unique,
                            social_id => SocialId
                        }}
                    }, Changeset);
                false ->
                    Changeset
            end;
        error ->
            Changeset
    end.

set_id(Changeset) ->
    case changeset:get_data(id, Changeset, undefined) of
        undefined ->
            Id = ebank_crypto:rand_int(),
            changeset:push_change(id, Id, Changeset);
        _ ->
            Changeset
    end.

hash_password(Changeset) ->
    case changeset:find_change(password, Changeset) of
        {ok, Password} ->
            Hash = ebank_crypto:strong_hash(Password),
            changeset:push_change(password, Hash, Changeset);
        error ->
            Changeset
    end.

set_created_at(Changeset) ->
    case changeset:get_data(created_at, Changeset, undefined) of
        undefined ->
            Now = calendar:universal_time(),
            changeset:push_change(created_at, Now, Changeset);
        _ ->
            Changeset
    end.
