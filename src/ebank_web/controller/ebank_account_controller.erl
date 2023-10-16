-module(ebank_account_controller).

%% API functions
-export([ fetch/2, insert/2, update/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

insert([], Req0) ->
    case ebank_server:get_json_body(Req0) of
        {ok, {Params0, Req1}} ->
            Schema = ebank_account:schema(),
            BinKeys = ebank_schema:fields_name_as_binary(Schema),
            Params1 = maps:with(BinKeys, Params0),
            Params = ebank_maps:binary_keys_to_existing_atom(Params1),
            case ebank_account:insert(Params) of
                {ok, AccountRecord} ->
                    Account = ebank_account:to_map(AccountRecord),
                    {ok, Req2} = ebank_server:set_json_body(Account, Req1),
                    Req = ebank_server:set_status_code(200, Req2),
                    {ok, Req};
                {error, {changeset, Changeset}} ->
                    Errors = lists:map(fun({Field, {Msg, _Meta}}) ->
                        {Field, Msg}
                    end, changeset:get_errors(Changeset)),
                    {ok, Req2} = ebank_server:set_json_body(Errors, Req1),
                    Req = ebank_server:set_status_code(400, Req2),
                    {ok, Req};
                % @todo: send error to log.
                {error, _Reason} ->
                    Req = ebank_server:set_status_code(500, Req0),
                    {ok, Req}
            end;
        % @todo: send error reason.
        {error, _Reason} ->
            Req = ebank_server:set_status_code(400, Req0),
            {ok, Req}
    end.

fetch(IdBin, Req0) ->
    case ebank_converter:binary_to_integer(IdBin) of
        {ok, Id} ->
            case ebank_account:fetch(Id) of
                {ok, AccountRecord} ->
                    Account = ebank_account:to_map(AccountRecord),
                    {ok, Req1} = ebank_server:set_json_body(Account, Req0),
                    Req = ebank_server:set_status_code(200, Req1),
                    {ok, Req};
                {error, enoent} ->
                    Req = ebank_server:set_status_code(404, Req0),
                    {ok, Req}
            end;
        {error, badarg} ->
            Req = ebank_server:set_status_code(404, Req0),
            {ok, Req}
    end.

update(IdBin, Req0) ->
    case ebank_converter:binary_to_integer(IdBin) of
        {ok, Id} ->
            % @todo: update data.
            Req1 = ebank_server:set_json_body(#{
                id => Id
            }, Req0),
            Req = ebank_server:set_status_code(200, Req1),
            {ok, Req};
        {error, badarg} ->
            Req = ebank_server:set_status_code(404, Req0),
            {ok, Req}
    end.
