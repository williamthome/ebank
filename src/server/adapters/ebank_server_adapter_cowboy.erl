-module(ebank_server_adapter_cowboy).

-behaviour(ebank_server_adapter).
-behaviour(cowboy_handler).

%% ebank_server_adapter callbacks
-export([ start/1 ]).

%% cowboy_handler callbacks
-export([ init/2 ]).

%% Types
-export_type([]).

-record(state, { router :: router() }).

-type router() :: module().

%% Macros
-define(LISTENER, ebank_http_listener).

%%----------------------------------------------------------------------
%% EBANK_SERVER_ADAPTER CALLBACKS
%%----------------------------------------------------------------------

start(Args) ->
    State = #state{
        router = maps:get(router, Args)
    },
    Routes = [{'_', ?MODULE, State}],
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

%%----------------------------------------------------------------------
%% COWBOY_HANDLER CALLBACKS
%%----------------------------------------------------------------------

init(Req, State) ->
    Method = normalize_method(cowboy_req:method(Req)),
    Path = normalize_path(cowboy_req:path(Req)),
    Router = State#state.router,
    {M, F, A} = Router:match(Method, Path),
    {ok, Res0} = M:F(A, Req),
    Res = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        <<"Hello, World!">>,
        Res0
    ),
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
