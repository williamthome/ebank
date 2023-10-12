-module(ebank_server).

%% API functions
-export([ start/0 ]).

%% Macros
-define(ADAPTER, (ebank_env:get_server(adapter))).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

start() ->
    ?ADAPTER:start(ebank_env:get_server(args)).
