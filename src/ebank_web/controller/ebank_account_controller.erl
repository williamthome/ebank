-module(ebank_account_controller).

%% API functions
-export([ fetch/2, insert/2, update/2 ]).

%%----------------------------------------------------------------------
%% API FUNCTIONS
%%----------------------------------------------------------------------

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

insert([], Req0) ->
    case ebank_server:get_json_body(Req0) of
        {ok, {Body, Req1}} ->
            % @todo: insert data.
            case ebank_server:set_json_body(Body, Req1) of
                {ok, Req2} ->
                    Req = ebank_server:set_status_code(200, Req2),
                    {ok, Req};
                {error, _Reason} ->
                    Req = ebank_server:set_status_code(500, Req0),
                    {ok, Req}
            end;
        {error, _Reason} ->
            Req = ebank_server:set_status_code(400, Req0),
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
