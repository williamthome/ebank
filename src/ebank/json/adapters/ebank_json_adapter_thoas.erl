-module(ebank_json_adapter_thoas).

-behaviour(ebank_json_adapter).

%% ebank_json_adapter callbacks
-export([ encode/1, decode/1 ]).

%%----------------------------------------------------------------------
%% EBANK_JSON_ADAPTER CALLBACKS
%%----------------------------------------------------------------------

encode(Term) ->
    try {ok, thoas:encode_to_iodata(Term)}
    catch
        _:{error, Reason} ->
            {error, Reason};
        _:Reason ->
            {error, Reason}
    end.

decode(Bin) ->
    thoas:decode(Bin).
