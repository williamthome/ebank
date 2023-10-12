-module(ebank_schema_transform).

%% API functions
-export([ parse_transform/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

parse_transform(Forms, _Options) ->
    FName = schema, FArity = 0,
    case parserl_trans:find_function(FName, FArity, Forms) of
        {true, FForm} ->
            parserl_trans:replace_function(
                FName, FArity, compile(FForm), Forms, []
            );
        false ->
            error({schema_enoent, FName, FArity})
    end.

%%----------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%----------------------------------------------------------------------

compile({function, _, _, _, [Clause0]} = Form) ->
    FBody = element(5, Clause0),
    Schema = parserl_trans:eval(FBody),
    Clause = setelement(5, Clause0, [parserl_trans:quote(Schema)]),
    setelement(5, Form, [Clause]).
