-module(ebank_app).

-behaviour(application).

%% application callbacks
-export([ start/2, stop/1 ]).

%%----------------------------------------------------------------------
%% APPLICATION CALLBACKS
%%----------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    ok = ebank_db:connect(),
    % @todo: migrations should be done via scripts.
    ok = ebank_migrator:migrate(up, 1),
    ebank_sup:start_link().

stop(_State) ->
    ok.
