-module(ebank_dsl_adapter).

%% Types
-export_type([ table/0, field_name/0, index/0, indexes/0, clause/0 ]).

-type table() :: ebank_db_adapter:table().
-type field_name() :: ebank_schema_field:name().
-type index() :: pos_integer().
-type indexes() :: #{table() => #{field_name() => index()}}.
-type clause() :: tuple().

%% Callbacks
-optional_callbacks([]).

-callback query(Clauses, Indexes) -> term()
    when Clauses :: [clause()]
       , Indexes :: indexes()
       .
