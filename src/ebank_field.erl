-module(ebank_field).

%% API functions
-export([ new/1
        , name/1
        , index/1
        , type/1
        , permitted/1
        , required/1
        ]).

%% Types
-export_type([ t/0, name/0, index/0 ]).

-record(field, { name :: atom()
               , index :: index()
               , type :: type()
               , permitted :: boolean()
               , required :: boolean()
               }).

-opaque t() :: #field{}.

-type name() :: atom().

-type index() :: 2..16_777_215.

-type type() :: changeset:type().

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

new(Args) ->
    #field{ name = maps:get(name, Args)
          , index = maps:get(index, Args)
          , type = maps:get(type, Args)
          , permitted = maps:get(permitted, Args, true)
          , required = maps:get(required, Args, false)
          }.

name(#field{name = Name}) ->
    Name.

index(#field{index = Index}) ->
    Index.

type(#field{type = Type}) ->
    Type.

permitted(#field{permitted = Permitted}) ->
    Permitted.

required(#field{required = Required}) ->
    Required.
