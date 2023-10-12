-module(ebank_server_adapter).

%% Callbacks
-optional_callbacks([]).

-callback start(Args) -> ok | {error, term()}
    when Args :: map().
