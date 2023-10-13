-module(ebank_account).

%% API functions
-export([ schema/0
        , changeset/2
        ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

schema() ->
    ebank_account_schema:schema().

changeset(Data, Params) ->
    ebank_account_schema:changeset(Data, Params).
