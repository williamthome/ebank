-module(ebank_schema_transform).

%% API functions
-export([ parse_transform/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

parse_transform(Forms, _Options) ->
    case all_loaded() of
        true ->
            case has_schema_fun(Forms) of
                {true, Form} ->
                    replace_schema_fun(Form, Forms);
                false ->
                    % @todo: emit a warning about no schema fun.
                    Forms
            end;
        false ->
            Forms
    end.

%%----------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%----------------------------------------------------------------------

all_loaded() ->
    case ensure_modules_loaded([ebank_modules]) of
        true ->
            ebank_modules:all_loaded([
                {ebank_schema, [{new, 1}]}
            ]);
        false ->
            false
    end.

ensure_modules_loaded(Modules) ->
    lists:all(fun(Mod) ->
        code:ensure_loaded(Mod) =:= {module, Mod}
    end, Modules).

has_schema_fun(Forms) ->
    ebank_parse_transform:find_function(schema, 0, Forms).

replace_schema_fun(Form, Forms) ->
    ebank_parse_transform:replace_function(schema, 0, compile(Form), Forms).

compile({function, _, _, _, [Clause0]} = Form) ->
    FBody = element(5, Clause0),
    Schema = ebank_parse_transform:eval_form(FBody),
    AST = [ebank_parse_transform:term_to_ast(Schema)],
    Clause = setelement(5, Clause0, AST),
    setelement(5, Form, [Clause]).
