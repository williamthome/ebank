-module(ebank_account).

-behaviour(ebank_model).

%% ebank_model callbacks
-export([ schema/0, changeset/2 ]).

%% Libs
-include("ebank_model.hrl").

%%----------------------------------------------------------------------
%% EBANK_MODEL CALLBACKS
%%----------------------------------------------------------------------

schema() ->
    ebank_schema:new(#{
        table => account,
        fields => [
            {id, #{
                type => integer,
                permitted => false
            }},
            {social_id, #{
                type => binary,
                required => true,
                indexed => true
            }},
            {name, #{
                type => binary,
                required => true
            }},
            {password, #{
                type => binary,
                required => true
            }},
            {created_at, #{
                type => datetime,
                permitted => false
            }}
        ]
    }).

changeset(Data, Params) ->
    Changeset = ebank_schema:changeset(Data, Params, schema()),
    case changeset:is_valid(Changeset) of
        true ->
            changeset:pipe(Changeset, [
                fun set_id/1,
                fun hash_password/1,
                fun set_created_at/1
            ]);
        false ->
            Changeset
    end.

%%----------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%----------------------------------------------------------------------

set_id(Changeset) ->
    case changeset:get_data(id, Changeset, undefined) of
        undefined ->
            Id = ebank_crypto:rand_int(),
            changeset:push_change(id, Id, Changeset);
        _ ->
            Changeset
    end.

hash_password(Changeset) ->
    case changeset:find_changes(password, Changeset) of
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
