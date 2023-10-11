-module(ebank_app).

-behaviour(application).

%% application callbacks
-export([ start/2, stop/1 ]).

%%----------------------------------------------------------------------
%% APPLICATION CALLBACKS
%%----------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    ok = ebank_db:connect(),
    ok = ebank_repo:create_tables(),
    ebank_sup:start_link().

stop(_State) ->
    ok.
