-module(ebank_field).

%% API functions
-export([ new/1
        , name/1
        , index/1
        ]).

%% Types
-export_type([ t/0, name/0, index/0 ]).

-record(field, { name :: atom()
               , index :: index()
               }).

-opaque t() :: #field{}.

-type name() :: atom().
-type index() :: 2..16_777_215.

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

new(Args) ->
    #field{ index = maps:get(index, Args) }.

name(#field{name = Name}) ->
    Name.

index(#field{index = Index}) ->
    Index.
