-module(ebank_modules).

%% API functions
-export([ ensure_loaded/1, function_exported/3, all_loaded/1 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

ensure_loaded(Module) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            true;
        {error, _} ->
            false
    end.

function_exported(Module, Fun, Arity) ->
    erlang:function_exported(Module, Fun, Arity).

all_loaded(ModsAndFuns) ->
    lists:all(fun(({Module, Funs})) ->
        ensure_loaded(Module) =:= true andalso
        lists:all(fun({Fun, Arity}) ->
            function_exported(Module, Fun, Arity)
        end, Funs)
    end, ModsAndFuns).
