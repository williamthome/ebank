-module(ebank_router).

%% API functions
-export([ match/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

%% Accounts

match(get, [ <<"accounts">>, Id ]) ->
    {ebank_account_controller, fetch, Id};
match(post, [ <<"accounts">> ]) ->
    {ebank_account_controller, insert, []};
match(patch, [ <<"accounts">>, Id ]) ->
    {ebank_account_controller, update, Id};

%% Errors

match(Method, Path) ->
    {ebank_error_controller, invalid_route, [Method, Path]}.
