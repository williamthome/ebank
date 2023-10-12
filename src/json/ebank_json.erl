-module(ebank_json).

%% API functions
-export([ encode/1, decode/1 ]).

%% Macros
-define(ADAPTER, (ebank_env:get_json(adapter))).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

encode(Term) ->
    ?ADAPTER:encode(Term).

decode(Bin) ->
    ?ADAPTER:decode(Bin).
