-module(ebank_schema_transform).

%% API functions
-export([ parse_transform/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

parse_transform(Forms, _Options) ->
    case ensure_schema_mod_loaded() of
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

ensure_schema_mod_loaded() ->
    code:ensure_loaded(ebank_schema),
    erlang:function_exported(ebank_schema, new, 1).

has_schema_fun(Forms) ->
    parserl_trans:find_function(schema, 0, Forms).

replace_schema_fun(Form, Forms) ->
    parserl_trans:replace_function(schema, 0, compile(Form), Forms, []).

compile({function, _, _, _, [Clause0]} = Form) ->
    FBody = element(5, Clause0),
    Schema = parserl_trans:eval(FBody),
    Clause = setelement(5, Clause0, [parserl_trans:quote(Schema)]),
    setelement(5, Form, [Clause]).
