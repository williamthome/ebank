-module(ebank_model_SUITE).

-behaviour(ebank_model).

% -include_lib("common_test/include/ct.hrl").

%% Test server callbacks
-export([ suite/0
        , all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

% ebank_model callbacks
-export([ schema/0 ]).

%% Test cases
-export([ fields/1
        , fields_name/1
        , field_by_name/1
        , field_index/1
        , get_field_value/1
        , set_field_value/1
        ]).

%%----------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Description: Returns list of tuples to set default properties
%%              for the suite.
%%----------------------------------------------------------------------
suite() ->
    [].

%%----------------------------------------------------------------------
%% Function: init_per_suite(Config0) -> Config1
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Initialization before the suite.
%%----------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%----------------------------------------------------------------------
%% Function: end_per_suite(Config) -> term()
%%
%% Config = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after the suite.
%%----------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%----------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) -> Config1
%%
%% TestCase = atom()
%%   Name of the test case that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Initialization before each test case.
%%----------------------------------------------------------------------
init_per_testcase(_Case, Config) ->
    Config.

%%----------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config) -> term()
%%
%% TestCase = atom()
%%   Name of the test case that is finished.
%% Config = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after each test case.
%%----------------------------------------------------------------------
end_per_testcase(_Case, _Config) ->
    ok.

%%----------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%%
%% Description: Returns the list of groups and test cases that
%%              are to be executed.
%%----------------------------------------------------------------------
all() ->
    [ fields, fields_name, field_by_name, field_index, get_field_value
    , set_field_value
    ].

%%----------------------------------------------------------------------
%% HELPERS
%%----------------------------------------------------------------------

-record(user, { name }).

schema() -> #{
    fields => #{
        name => #{
            index => #user.name
        }
    }
}.

%%----------------------------------------------------------------------
%% TEST CASES
%%----------------------------------------------------------------------

fields(_Config) ->
    maps:get(fields, schema()) =:=
        ebank_model:fields(?MODULE),
    ok.

fields_name(_Config) ->
    true =:=
        lists:all(fun(Name) ->
            lists:member(Name, [name])
        end, ebank_model:fields_name(?MODULE)),
    ok.

field_by_name(_Config) ->
    maps:get(name, maps:get(fields, schema())) =:=
        ebank_model:field_by_name(name, ?MODULE),
    ok.

field_index(_Config) ->
    2 =:= ebank_model:field_index(name, ?MODULE),
    ok.

get_field_value(_Config) ->
    <<"Joe">> =:=
        ebank_model:get_field_value(name, #user{name = <<"Joe">>}, ?MODULE),
    ok.

set_field_value(_Config) ->
    #user{name = <<"Joe">>} =
        ebank_model:set_field_value(name, <<"Joe">>, #user{}, ?MODULE),
    ok.
