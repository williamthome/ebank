-module(ebank).

%% API functions
-export([ get_env/0, get_env/1, get_env/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

get_env() ->
    proplists:to_map(application:get_all_env(?MODULE)).

get_env(Key) ->
    maps:get(Key, get_env()).

get_env(Key, Default) ->
    maps:get(Key, get_env(), Default).
