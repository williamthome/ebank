-module(ebank_error_controller).

%% API functions
-export([ invalid_route/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

% @todo: set error response.
invalid_route([_Method, _Path], Req) ->
    {ok, Req}.
