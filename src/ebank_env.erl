-module(ebank_env).
-compile({no_auto_import,[get/1]}).

%% API functions
-export([ get_all/0, get/1, get/2 ]).
-export([ get_db/0, get_db/1, get_db/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

get_all() ->
    proplists:to_map(application:get_all_env(ebank)).

get(Key) ->
    maps:get(Key, get_all()).

get(Key, Default) ->
    maps:get(Key, get_all(), Default).

get_db() ->
    get(db).

get_db(Key) ->
    maps:get(Key, get_db()).

get_db(Key, Default) ->
    maps:get(Key, get_db(), Default).
