-module(ebank_handler).

%% API functions
-export([ handle/3 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

handle(Method, Path, Req0) ->
    {{Controller, Fun, Args}, Opts} = ebank_router:match(Method, Path),
    case resolve_opts(Opts, Req0) of
        {continue, Req} ->
            Controller:Fun(Args, Req);
        {halt, Req} ->
            {ok, Req}
    end.

%%----------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%----------------------------------------------------------------------

resolve_opts([Opt | T], Req0) ->
    case resolve_opt(Opt, Req0) of
        {continue, Req} ->
            resolve_opts(T, Req);
        {halt, Req} ->
            {halt, Req}
    end;
resolve_opts([], Req) ->
    {continue, Req}.

resolve_opt({middlewares, Middlewares}, Req) ->
    resolve_middlewares(Middlewares, Req).

resolve_middlewares([Middleware | T], Req0) ->
    case resolve_middleware(Middleware, Req0) of
        {continue, Req} ->
            resolve_middlewares(T, Req);
        {halt, Req} ->
            {halt, Req}
    end;
resolve_middlewares([], Req) ->
    {continue, Req}.

% @todo: resolve middlewares.
resolve_middleware({auth, _Args}, Req) ->
    {continue, Req}.
