-module(ebank_account).

-behaviour(ebank_model).

%% ebank_model callbacks
-export([ schema/0 ]).

%%----------------------------------------------------------------------
%% EBANK_MODEL CALLBACKS
%%----------------------------------------------------------------------

% @todo: expand via parse transform.
schema() ->
    ebank_schema:new(#{
        table => account,
        fields => [
            {id, #{
                type => integer,
                permitted => false,
                default => fun(Changeset) ->
                    case changeset:get_change(id, Changeset, undefined) of
                        undefined ->
                            % @@todo: unique id.
                            Id = rand:uniform(1_000),
                            changeset:push_change(id, Id, Changeset);
                        _ ->
                            Changeset
                    end
                end
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
                permitted => false,
                default => fun(Changeset) ->
                    case changeset:get_change(created_at, Changeset, undefined) of
                        undefined ->
                            Now = calendar:universal_time(),
                            changeset:push_change(created_at, Now, Changeset);
                        _ ->
                            Changeset
                    end
                end
            }}
        ]
    }).
