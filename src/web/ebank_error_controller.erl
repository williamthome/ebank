-module(ebank_error_controller).

%% API functions
-export([ invalid_route/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

% @todo: improve response.
invalid_route([_Method, _Path], Req0) ->
    Req = ebank_server:set_status_code(404, Req0),
    {ok, Req}.
