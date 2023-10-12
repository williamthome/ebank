-module(ebank_model).

%% Callbacks
-optional_callbacks([]).

-callback schema() -> ebank_schema:t().
