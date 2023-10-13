-module(ebank_schema_SUITE).

% -include_lib("common_test/include/ct.hrl").

%% Test server callbacks
-export([ suite/0
        , all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

% ebank_schema callbacks
-export([ schema/0 ]).

%% Test cases
-export([ fields_name/1
        , field/1
        , field_index/1
        , get_field_value/1
        , set_field_value/1
        , changeset/1
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
    [ fields_name
    , field
    , field_index
    , get_field_value
    , set_field_value
    , changeset
    ].

%%----------------------------------------------------------------------
%% HELPERS
%%----------------------------------------------------------------------

-record(user, { name }).

schema() ->
    ebank_schema:new(#{
        table => user,
        fields => [
            {name, #{
                type => binary,
                required => true,
                readonly => false
            }}
        ]
    }).

%%----------------------------------------------------------------------
%% TEST CASES
%%----------------------------------------------------------------------

fields_name(_Config) ->
    true =
        lists:all(fun(Name) ->
            lists:member(Name, [name])
        end, ebank_schema:fields_name(schema())),
    ok.

field(_Config) ->
    Args = #{ name => name
            , index => #user.name - 1
            , type => binary
            , readonly => false
            , required => true
            },
    true =
        ebank_schema_field:new(Args) =:= ebank_schema:field(name, schema()),
    ok.

field_index(_Config) ->
    1 = ebank_schema:field_index(name, schema()),
    ok.

get_field_value(_Config) ->
    <<"Joe">> =
        ebank_schema:get_field_value(name, #user{name = <<"Joe">>}, schema()),
    ok.

set_field_value(_Config) ->
    #user{name = <<"Joe">>} =
        ebank_schema:set_field_value(name, <<"Joe">>, #user{}, schema()),
    ok.

changeset(_Config) ->
    false = changeset:is_valid(
        ebank_schema:changeset(#{}, #{}, schema())
    ),

    true = changeset:is_valid(
        ebank_schema:changeset(#{}, #{name => <<"Joe">>}, schema())
    ),

    ok.
