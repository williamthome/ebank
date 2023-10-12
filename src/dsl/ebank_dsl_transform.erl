-module(ebank_dsl_transform).

%% API functions
-export([ parse_transform/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

parse_transform(Forms, _Options) ->
    case has_model_attr(Forms) of
        {true, Model} ->
            case is_schema_fun_exported(Model) of
                true ->
                    case has_query_attr(Forms) of
                        {true, QueryFuns} ->
                            replace_schema_fun(QueryFuns, Forms);
                        false ->
                            % @todo: emit a warning about no query attribute.
                            Forms
                    end;
                false ->
                    % @todo: emit a warning about no schema fun.
                    Forms
            end;
        false ->
            % @todo: emit a warning about no model attribute.
            Forms
    end.

%%----------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%----------------------------------------------------------------------

has_model_attr(Forms) ->
    case parserl_trans:find_attribute(model, Forms) of
        {true, {attribute, _, model, Model}} ->
            {true, Model};
        false ->
            false
    end.

is_schema_fun_exported(Model) ->
    code:ensure_loaded(Model),
    erlang:function_exported(Model, schema, 0).

has_query_attr(Forms) ->
    case parserl_trans:find_attribute(query, Forms) of
        {true, {attribute, _, query, QueryFuns}} ->
            {true, QueryFuns};
        false ->
            false
    end.

replace_schema_fun(QueryFuns, Forms) ->
    lists:foldl(fun({FName, FArity}, Acc) ->
        case parserl_trans:find_function(FName, FArity, Forms) of
            {true, FForm} ->
                parserl_trans:replace_function(
                    FName, FArity, compile(FForm), Acc, []
                );
            false ->
                error({dsl_enoent, FName, FArity})
        end
    end, Forms, QueryFuns).

compile({function, _, _, _, [Clause0]} = Form) ->
    FBody = element(5, Clause0),
    Query = parserl_trans:eval(FBody),
    Anno = element(2, Clause0),
    Clause = setelement(5, Clause0, [{string, Anno, Query}]),
    setelement(5, Form, [Clause]).
