-module(ebank_dsl_transform).

%% API functions
-export([ parse_transform/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

parse_transform(Forms, _Options) ->
    case parserl_trans:find_attribute(query, Forms) of
        {true, {attribute, _, query, Queries}} ->
            lists:foldl(fun({FName, FArity}, Acc) ->
                case parserl_trans:find_function(FName, FArity, Forms) of
                    {true, FForm} ->
                        parserl_trans:replace_function(
                            FName, FArity, compile(FForm), Acc, []
                        );
                    false ->
                        error({dsl_enoent, FName, FArity})
                end
            end, Forms, Queries);
        false ->
            % @todo: emit a warning about no query attribute found.
            Forms
    end.

%%----------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%----------------------------------------------------------------------

compile({function, _, _, _, [Clause0]} = Form) ->
    FBody = element(5, Clause0),
    Query = parserl_trans:eval(FBody),
    Anno = element(2, Clause0),
    Clause = setelement(5, Clause0, [{string, Anno, Query}]),
    setelement(5, Form, [Clause]).
