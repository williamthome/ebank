-module(ebank_account).

-behaviour(ebank_model).

%% ebank_model callbacks
-export([ schema/0 ]).

%% API functions
-export([ changeset/2 ]).

%%----------------------------------------------------------------------
%% EBANK_MODEL CALLBACKS
%%----------------------------------------------------------------------

schema() ->
    ebank_schema:new(#{
        table => account,
        fields => #{
            id => #{
                type => integer,
                required => true
                % permitted => false
            },
            social_id => #{
                type => binary,
                required => true,
                indexed => true
            },
            name => #{
                type => binary,
                required => true
            },
            password => #{
                type => binary,
                required => true
            }
            % created_at => #{
            %     type => datetime,
            %     permitted => false
            % }
        }
    }).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

changeset(Data, Params) ->
    ebank_schema:changeset(Data, Params, schema()).
