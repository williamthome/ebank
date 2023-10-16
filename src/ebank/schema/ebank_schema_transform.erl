-module(ebank_schema_transform).

%% API functions
-export([ parse_transform/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

parse_transform(Forms0, _Options) ->
    case collect_attributes(Forms0) of
        {ok, #{schema := SchemaArgs}} ->
            case all_loaded() of
                true ->
                    Forms1 = compile_schema(SchemaArgs, Forms0),
                    Forms = ebank_parse_transform:remove_attribute(schema, Forms1),
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

all_loaded() ->
    Deps = [parserl_trans, ebank_parse_transform, ebank_modules],
    case ensure_modules_loaded(Deps) of
        true ->
            ebank_modules:all_loaded([
                {ebank_schema_field, [{new, 1}]},
                {ebank_schema, [{new, 1}]}
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
            {ok, ebank_parse_transform:collect_attributes([schema], Forms)};
        false ->
            error
    end.

compile_schema(Args, Forms) ->
    Schema = ebank_schema:new(Args),
    Fun = "schema() -> _@schema.",
    Bindings = #{schema => Schema},
    ebank_parse_transform:insert_function(Fun, Bindings, Forms, [export]).
