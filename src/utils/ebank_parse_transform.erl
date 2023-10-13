-module(ebank_parse_transform).

%% API functions
-export([ find_attribute/2
        , collect_attributes/2
        , find_function/3
        , replace_function/4
        , eval_form/1
        , term_to_ast/1
        ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

find_attribute(Attr, Forms) ->
    case parserl_trans:find_attribute(Attr, Forms) of
        {true, {attribute, _, Attr, Props}} ->
            {true, Props};
        false ->
            false
    end.

collect_attributes(Attrs, Forms) ->
    lists:foldl(fun(Attr, Acc) ->
        case find_attribute(Attr, Forms) of
            {true, Props} ->
                Acc#{Attr => Props};
            false ->
                Acc
        end
    end, #{}, Attrs).

find_function(Name, Arity, Forms) ->
    parserl_trans:find_function(Name, Arity, Forms).

replace_function(Name, Arity, Form, Forms) ->
    parserl_trans:replace_function(Name, Arity, Form, Forms, []).

eval_form(Form) ->
    parserl_trans:eval(Form).

term_to_ast(Term) ->
    parserl_trans:quote(Term).
