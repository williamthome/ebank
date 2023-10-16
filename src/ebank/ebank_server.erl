-module(ebank_server).

%% API functions
-export([ start/0
        , put_headers/2
        , set_status_code/2
        , get_body/1
        , set_body/2
        , get_json_body/1
        , set_json_body/2
        , upsert_request/3
        , fetch_request/3
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

upsert_request(Fun, Schema, Req0) ->
    case ebank_server:get_json_body(Req0) of
        {ok, {Params0, Req1}} ->
            BinKeys = ebank_schema:fields_name_as_binary(Schema),
            Params1 = maps:with(BinKeys, Params0),
            Params = ebank_maps:binary_keys_to_existing_atom(Params1),
            case Fun(Params) of
                {ok, Record} ->
                    Data = ebank_schema:to_map(Record, Schema),
                    {ok, Req2} = ebank_server:set_json_body(Data, Req1),
                    ebank_server:set_status_code(200, Req2);
                {error, {changeset, Changeset}} ->
                    Errors = lists:map(fun({Field, {Msg, _Meta}}) ->
                        {Field, Msg}
                    end, changeset:get_errors(Changeset)),
                    {ok, Req2} = ebank_server:set_json_body(Errors, Req1),
                    ebank_server:set_status_code(400, Req2);
                % @todo: send error to log.
                {error, _Reason} ->
                    ebank_server:set_status_code(500, Req1)
            end;
        {error, badarg} ->
            ebank_server:set_status_code(404, Req0)
    end.

fetch_request(FetchFun, Schema, Req0) ->
    case FetchFun() of
        {ok, Record} ->
            Data = ebank_schema:to_map(Record, Schema),
            {ok, Req1} = ebank_server:set_json_body(Data, Req0),
            ebank_server:set_status_code(200, Req1);
        {error, enoent} ->
            ebank_server:set_status_code(404, Req0)
    end.
