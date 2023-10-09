-module(ebank_app).

-behaviour(application).

%% application callbacks
-export([ start/2, stop/1 ]).

%%----------------------------------------------------------------------
%% APPLICATION CALLBACKS
%%----------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    ebank_sup:start_link(ebank:get_env()).

stop(_State) ->
    ok.
