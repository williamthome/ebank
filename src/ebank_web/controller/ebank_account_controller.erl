-module(ebank_account_controller).

%% API functions
-export([ fetch/2, insert/2, update/2 ]).

%% Macros
-define(SCHEMA, ebank_account:schema()).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

insert([], Req0) ->
    Insert = fun(Params) -> ebank_account:insert(Params) end,
    Req = ebank_server:upsert_request(Insert, ?SCHEMA, Req0),
    {ok, Req}.

fetch(IdBin, Req0) ->
    case ebank_converter:binary_to_integer(IdBin) of
        {ok, Id} ->
            Fetch = fun() -> ebank_account:fetch(Id) end,
            Req = ebank_server:fetch_request(Fetch, ?SCHEMA, Req0),
            {ok, Req};
        {error, badarg} ->
            Req = ebank_server:set_status_code(404, Req0),
            {ok, Req}
    end.

update(IdBin, Req0) ->
    case ebank_converter:binary_to_integer(IdBin) of
        {ok, Id} ->
            Update = fun(Params) -> ebank_account:update(Id, Params) end,
            Req = ebank_server:upsert_request(Update, ?SCHEMA, Req0),
            {ok, Req};
        {error, badarg} ->
            Req = ebank_server:set_status_code(404, Req0),
            {ok, Req}
    end.
