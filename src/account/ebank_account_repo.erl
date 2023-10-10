-module(ebank_account_repo).

%% API functions
-export([ insert/1 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

insert(Params) ->
    Changeset = ebank_account:changeset(#{}, Params),
    ebank_repo:insert(Changeset, ebank_account:schema()).
