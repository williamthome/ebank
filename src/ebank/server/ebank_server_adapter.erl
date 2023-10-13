-module(ebank_server_adapter).

%% Types
-export_type([ request/0, headers/0, status_code/0, body/0 ]).

-type request() :: term().
-type headers() :: #{binary() => binary()}.
-type status_code() :: 200..500.
-type body() :: binary().

%% Callbacks
-optional_callbacks([]).

-callback start(Args) -> ok | {error, term()}
    when Args :: map().

-callback put_headers(Headers, Req) -> request()
    when Headers :: headers()
       , Req :: request()
       .

-callback set_status_code(StatusCode, Req) -> request()
    when StatusCode :: status_code()
       , Req :: request()
       .

-callback get_body(request()) -> {binary(), request()}.

-callback set_body(Body, Req) -> request()
    when Body :: body()
       , Req :: request()
       .
