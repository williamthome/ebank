-module(ebank_sup).

-behaviour(supervisor).

%% API functions
-export([ start_link/0 ]).

%% supervisor callbacks
-export([ init/1 ]).

%% Macros
-define(SERVER, ?MODULE).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%----------------------------------------------------------------------
%% SUPERVISOR CALLBACKS
%%----------------------------------------------------------------------

init([]) ->
    SupFlags = #{ strategy => one_for_all
                , intensity => 0
                , period => 1
                },

    ChildSpecs = [ ],

    {ok, {SupFlags, ChildSpecs}}.
