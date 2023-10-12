-module(ebank_model).

%% Types
-export_type([ schema/0, changeset/0, data/0 ]).

-type schema() :: ebank_schema:t().
-type changeset() :: ebank_schema:changeset().
-type data() :: ebank_schema:data().

%% Callbacks
-optional_callbacks([]).

-callback schema() -> schema().

-callback changeset(Data, Params) -> changeset()
    when Data :: data()
       , Params :: data()
       .
