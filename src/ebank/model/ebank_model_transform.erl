-module(ebank_model_transform).

%% API functions
-export([ parse_transform/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

parse_transform(Forms0, _Options) ->
    case collect_attributes(Forms0) of
        {ok, #{model := ModelAttr}} ->
            Schema = maps:get(schema, ModelAttr),
            case all_loaded(Schema) of
                true ->
                    QueryFuns = maps:get(queries, ModelAttr, []),
                    Forms1 = compile_queries(Forms0, QueryFuns),
                    Forms = ebank_parse_transform:remove_attribute(model, Forms1),
                    parserl_trans:restore(Forms);
                false ->
                    Forms0
            end;
        {ok, _} ->
            % @todo: emit a warning about missing attributes.
            Forms0;
        error ->
            Forms0
    end.

%%----------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%----------------------------------------------------------------------

all_loaded(Schema) ->
    Deps = [ebank_modules],
    case ensure_modules_loaded(Deps) of
        true ->
            ebank_modules:all_loaded([
                {ebank_qlc, [{query, 3}]},
                {Schema, [{schema, 0}]}
            ]);
        false ->
            false
    end.

ensure_modules_loaded(Modules) ->
    lists:all(fun(Mod) ->
        code:ensure_loaded(Mod) =:= {module, Mod}
    end, Modules).

collect_attributes(Forms) ->
    Deps = [parserl_trans, ebank_parse_transform],
    case ensure_modules_loaded(Deps) of
        true ->
            {ok, ebank_parse_transform:collect_attributes([model], Forms)};
        false ->
            error
    end.

compile_queries(Forms, QueryFuns) ->
    lists:foldl(fun({FName, FArity}, Acc) ->
        replace_query_fun(FName, FArity, Acc)
    end, Forms, QueryFuns).

replace_query_fun(FName, FArity, Forms) ->
    case ebank_parse_transform:find_function(FName, FArity, Forms) of
        {true, FForm} ->
            ebank_parse_transform:replace_function(
                FName, FArity, compile_query(FForm), Forms
            );
        false ->
            error({missing_query, FName, FArity})
    end.

compile_query({function, _, _, _, [Clause0]} = Form) ->
    FBody = element(5, Clause0),
    Query = ebank_parse_transform:eval_form(FBody),
    Anno = element(2, Clause0),
    AST = [{string, Anno, Query}],
    Clause = setelement(5, Clause0, AST),
    setelement(5, Form, [Clause]).
