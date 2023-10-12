-module(ebank_schema_transform).

%% API functions
-export([ parse_transform/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

% @todo: expand/compile schemas.
parse_transform(Forms, _Options) ->
    Forms.
