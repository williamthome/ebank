-module(ebank_server).

%% API functions
-export([ start/0
        , put_headers/2
        , set_status_code/2
        , get_body/1
        , set_body/2
        , get_json_body/1
        , set_json_body/2
        ]).

%% Macros
-define(ADAPTER, (ebank_env:get_server(adapter))).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

start() ->
    ?ADAPTER:start(ebank_env:get_server(args)).

put_headers(Headers, Req) ->
    ?ADAPTER:put_headers(Headers, Req).

set_status_code(StatusCode, Req) ->
    ?ADAPTER:set_status_code(StatusCode, Req).

get_body(Req) ->
    ?ADAPTER:get_body(Req).

set_body(Body, Req) ->
    ?ADAPTER:set_body(Body, Req).

get_json_body(Req0) ->
    {Body, Req} = get_body(Req0),
    case ebank_json:decode(Body) of
        {ok, Term} ->
            {ok, {Term, Req}};
        {error, Reason} ->
            {error, Reason}
    end.

set_json_body(Term, Req0) ->
    case ebank_json:encode(Term) of
        {ok, Body} ->
            Req1 = set_body(Body, Req0),
            Req = put_headers(#{
                <<"content-type">> => <<"application/json; charset=utf-8">>
            }, Req1),
            {ok, Req};
        {error, Reason} ->
            {error, Reason}
    end.
