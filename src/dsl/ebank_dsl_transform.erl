-module(ebank_dsl_transform).

%% API functions
-export([ parse_transform/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

parse_transform(Forms, _Options) ->
    case collect_attributes(Forms) of
        {ok, #{model := Model, query := QueryFuns}} ->
            case all_loaded(Model) of
                true ->
                    replace_schema_fun(QueryFuns, Forms);
                false ->
                    Forms
            end;
        {ok, _} ->
            % @todo: emit a warning about missing attributes.
            Forms;
        {error, _} ->
            Forms
    end.

%%----------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%----------------------------------------------------------------------

all_loaded(Model) ->
    case ensure_modules_loaded([ebank_modules]) of
        true ->
            ebank_modules:all_loaded([
                {ebank_qlc, [{query, 3}]},
                {Model, [{schema, 0}]}
            ]);
        false ->
            false
    end.

ensure_modules_loaded(Modules) ->
    lists:all(fun(Mod) ->
        code:ensure_loaded(Mod) =:= {module, Mod}
    end, Modules).

collect_attributes(Forms) ->
    case code:ensure_loaded(ebank_parse_transform) of
        {module, ebank_parse_transform} ->
            {ok, ebank_parse_transform:collect_attributes([model, query], Forms)};
        {error, Reason} ->
            {error, Reason}
    end.

replace_schema_fun(QueryFuns, Forms) ->
    lists:foldl(fun({FName, FArity}, Acc) ->
        case ebank_parse_transform:find_function(FName, FArity, Forms) of
            {true, FForm} ->
                ebank_parse_transform:replace_function(
                    FName, FArity, compile(FForm), Acc
                );
            false ->
                error({missing_query, FName, FArity})
        end
    end, Forms, QueryFuns).

compile({function, _, _, _, [Clause0]} = Form) ->
    FBody = element(5, Clause0),
    Query = ebank_parse_transform:eval_form(FBody),
    Anno = element(2, Clause0),
    AST = [{string, Anno, Query}],
    Clause = setelement(5, Clause0, AST),
    setelement(5, Form, [Clause]).
