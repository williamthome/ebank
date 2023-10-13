-module(ebank_json_adapter).

%% Types
-export_type([ encode_arg/0, decode_arg/0 ]).

-type encode_arg() :: term().
-type decode_arg() :: binary().

%% Callbacks
-optional_callbacks([]).

-callback encode(encode_arg()) -> {ok, iodata()} | {error, term()}.

-callback decode(decode_arg()) -> {ok, term()} | {error, term()}.
