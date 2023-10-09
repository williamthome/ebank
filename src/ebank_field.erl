-module(ebank_field).

%% API functions
-export([ new/1, index/1 ]).

%% Types
-export_type([ t/0, index/0 ]).

-record(field, { index :: index() }).

-opaque t() :: #field{}.

-type index() :: 2..16_777_215.

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

new(Args) ->
    #field{ index = maps:get(index, Args) }.

index(#field{index = Index}) ->
    Index.
