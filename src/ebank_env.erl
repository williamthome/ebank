-module(ebank_env).
-compile({no_auto_import,[get/1]}).

%% API functions
-export([ get_all/0, get/1, get/2 ]).
-export([ get_server/0, get_server/1, get_server/2 ]).
-export([ get_db/0, get_db/1, get_db/2 ]).
-export([ get_dsl/0, get_dsl/1, get_dsl/2 ]).
-export([ get_crypto/0, get_crypto/1, get_crypto/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

get_all() ->
    proplists:to_map(application:get_all_env(ebank)).

get(Key) ->
    maps:get(Key, get_all()).

get(Key, Default) ->
    maps:get(Key, get_all(), Default).

% server

get_server() ->
    get(server).

get_server(Key) ->
    maps:get(Key, get_server()).

get_server(Key, Default) ->
    maps:get(Key, get_server(), Default).

% db

get_db() ->
    get(db).

get_db(Key) ->
    maps:get(Key, get_db()).

get_db(Key, Default) ->
    maps:get(Key, get_db(), Default).

% dsl

get_dsl() ->
    get(dsl).

get_dsl(Key) ->
    maps:get(Key, get_dsl()).

get_dsl(Key, Default) ->
    maps:get(Key, get_dsl(), Default).

% crypto

get_crypto() ->
    get(crypto).

get_crypto(Key) ->
    maps:get(Key, get_crypto()).

get_crypto(Key, Default) ->
    maps:get(Key, get_crypto(), Default).
