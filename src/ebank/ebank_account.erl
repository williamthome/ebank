-module(ebank_account).

%% schema functions
-export([ schema/0, changeset/2, to_map/1 ]).

%% model functions
-export([ insert/1, fetch/1, update/2 ]).

%%----------------------------------------------------------------------
%% SCHEMA FUNCTIONS
%%----------------------------------------------------------------------

schema() ->
    ebank_account_schema:schema().

changeset(Data, Params) ->
    ebank_account_schema:changeset(Data, Params).

to_map(Account) ->
    ebank_account_schema:to_map(Account).

%%----------------------------------------------------------------------
%% MODEL FUNCTIONS
%%----------------------------------------------------------------------

insert(Params) ->
    ebank_account_model:insert(Params).

fetch(Id) ->
    ebank_account_model:fetch(Id).

update(Id, Params) ->
    ebank_account_model:update(Id, Params).
