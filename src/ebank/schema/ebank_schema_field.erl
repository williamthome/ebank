-module(ebank_schema_field).

%% API functions
-export([ new/1
        , name/1
        , index/1
        , type/1
        , readonly/1
        , required/1
        , indexed/1
        , redacted/1
        ]).

%% Types
-export_type([ t/0, name/0, index/0, type/0 ]).

-record(field, { name :: atom()
               , index :: index()
               , type :: type()
               , readonly :: boolean()
               , required :: boolean()
               , indexed :: boolean()
               , redacted :: boolean()
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
          , readonly = maps:get(readonly, Args, false)
          , required = maps:get(required, Args, false)
          , indexed = maps:get(indexed, Args, false)
          , redacted = maps:get(redacted, Args, false)
          }.

name(#field{name = Name}) ->
    Name.

index(#field{index = Index}) ->
    Index.

type(#field{type = Type}) ->
    Type.

readonly(#field{readonly = ReadOnly}) ->
    ReadOnly.

required(#field{required = Required}) ->
    Required.

indexed(#field{indexed = Indexed}) ->
    Indexed.

redacted(#field{redacted = Redacted}) ->
    Redacted.
