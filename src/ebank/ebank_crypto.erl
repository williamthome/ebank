-module(ebank_crypto).

%% API functions
-export([ rand_int/0, rand_bytes/0, strong_hash/1, is_hash_equals/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

rand_int() ->
    do_rand_int(1000).

rand_bytes() ->
    do_rand_bytes(32).

strong_hash(Bin) ->
    do_strong_hash(Bin, 1000, 32).

is_hash_equals(A, B) ->
    crypto:hash_equals(A, B).

%%----------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%----------------------------------------------------------------------

% @note: erlang:timestamp() has microseconds precision.
do_rand_int(Range) ->
    erlang:phash2({erlang:timestamp(), node()}, Range).

do_rand_bytes(Range) ->
    Chars = "abcdefghijklmnopqrstuvwxyz1234567890!@",
    do_rand_bytes(Range, Chars, _CharsLength = 38).

% @note: CharsLength = length(Chars),
% @see: https://stackoverflow.com/a/30646376/14166584
do_rand_bytes(Range, Chars, CharsLength) ->
    lists:map(fun(_) ->
        Index = rand:uniform(CharsLength),
        lists:nth(Index, Chars)
    end, lists:seq(1, Range)).

% @note: Recommended for iterations is 1000.
% @see: https://nishothan-17.medium.com/pbkdf2-hashing-algorithm-841d5cc9178d
do_strong_hash(Bin, Iterations, Bits) ->
    crypto:pbkdf2_hmac(sha256, Bin, salt(), Iterations, Bits).

salt() ->
    list_to_binary(ebank_env:get_crypto(salt)).
