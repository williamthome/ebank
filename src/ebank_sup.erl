
-module(ebank_sup).

-behaviour(supervisor).

%% API functions
-export([ start_link/1 ]).

%% supervisor callbacks
-export([ init/1 ]).

%% Macros
-define(SERVER, ?MODULE).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%%----------------------------------------------------------------------
%% SUPERVISOR CALLBACKS
%%----------------------------------------------------------------------

init(Args) ->
    SupFlags = #{ strategy => one_for_all
                , intensity => 0
                , period => 1
                },

    DbArgs = maps:get(db, Args),
    DbSpec = #{ id => ebank_db
              , start => {ebank_db, start_link, [DbArgs] }
              },

    ChildSpecs = [ DbSpec ],

    {ok, {SupFlags, ChildSpecs}}.
