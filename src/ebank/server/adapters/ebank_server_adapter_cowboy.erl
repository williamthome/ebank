-module(ebank_server_adapter_cowboy).

-behaviour(ebank_server_adapter).
-behaviour(cowboy_handler).

%% ebank_server_adapter callbacks
-export([ start/1
        , put_headers/2
        , set_status_code/2
        , set_body/2
        , get_body/1
        ]).

%% cowboy_handler callbacks
-export([ init/2 ]).

%% Macros
-define(LISTENER, ebank_http_listener).

%%----------------------------------------------------------------------
%% EBANK_SERVER_ADAPTER CALLBACKS
%%----------------------------------------------------------------------

start(Args) ->
    Routes = [{'_', ?MODULE, []}],
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    RanchOpts = [
        {port, maps:get(port, Args, 8080)}
    ],
    Opts = #{env => #{dispatch => Dispatch}},
    case cowboy:start_clear(?LISTENER, RanchOpts, Opts) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

put_headers(Headers, Req) ->
    cowboy_req:set_resp_headers(Headers, Req).

set_status_code(StatusCode, Req) ->
    cowboy_req:reply(StatusCode, Req).

set_body(Body, Req) ->
    cowboy_req:set_resp_body(Body, Req).

% @todo: body stream.
get_body(Req0) ->
    {ok, Body, Req} = cowboy_req:read_body(Req0, #{length => infinity}),
    {Body, Req}.

%%----------------------------------------------------------------------
%% COWBOY_HANDLER CALLBACKS
%%----------------------------------------------------------------------

init(Req, State) ->
    Method = normalize_method(cowboy_req:method(Req)),
    Path = normalize_path(cowboy_req:path(Req)),
    {ok, Res} = ebank_handler:handle(Method, Path, Req),
    {ok, Res, State}.

%%----------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%----------------------------------------------------------------------

normalize_method(<<"GET">>) -> get;
normalize_method(<<"POST">>) -> post;
normalize_method(<<"PATCH">>) -> patch;
normalize_method(<<"DELETE">>) -> delete;
normalize_method(<<"PUT">>) -> put;
normalize_method(<<"CONNECT">>) -> connect;
normalize_method(<<"HEAD">>) -> head;
normalize_method(<<"OPTIONS">>) -> options;
normalize_method(<<"TRACE">>) -> trace.

normalize_path(Path) ->
    binary:split(Path, <<"/">>, [global, trim_all]).
