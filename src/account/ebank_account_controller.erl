-module(ebank_account_controller).

%% API functions
-export([ fetch/2, insert/2, update/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

fetch(IdBin, Req) ->
    case ebank_converter:binary_to_integer(IdBin) of
        {ok, _Id} ->
            {ok, Req};
        {error, badarg} ->
            % @todo: set error response.
            {ok, Req}
    end.

insert([], Req) ->
    {ok, Req}.

update(IdBin, Req) ->
    case ebank_converter:binary_to_integer(IdBin) of
        {ok, _Id} ->
            {ok, Req};
        {error, badarg} ->
            % @todo: set error response.
            {ok, Req}
    end.
