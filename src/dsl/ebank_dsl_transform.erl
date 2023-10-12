-module(ebank_dsl_transform).

%% API functions
-export([ parse_transform/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

% @todo: expand/compile queries.
parse_transform(Forms, _Options) ->
    Forms.
