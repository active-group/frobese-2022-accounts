%% This module represents the database layer

-module(database).
-include("data.hrl").
-export([init_database/0, write/2, read_all/2,
         put_account/1, get_account/1, get_all_accounts/0,
         unique_account_number/0, atomically/1]).

close_tables() ->
    dets:close(account),
    dets:close(table_id).

%% destroy tables in case they already existed
destroy_tables() ->
    file:delete("account.dets"),
    file:delete("table_id.dets").

% unfortunately, delete_table doesn't always work such that create_table doesn't fail, so don't check return value
create_tables() ->
    {ok, account} = dets:open_file(account, [{type, set}, {file, "account.dets"}]),
    {ok, table_id} = dets:open_file(table_id, [{type, set}, {file, "table_id.dets"}]),
    dets:insert(table_id, {account, 0}).

init_database() ->
    close_tables(),
    destroy_tables(),
    create_tables(),
    ok.

write(Table, Tuple) ->
    ok = dets:insert(Table, Tuple),
    ok.

-spec read_one(dets:tab_name(), unique_id(), fun((tuple()) -> Obj)) -> {ok, Obj} | {error, not_found | more_than_one}.
read_one(Table, Id, Deserialize) ->
    Res = dets:lookup(Table, Id),
    case Res of
        [Tuple] -> {ok, Deserialize(Tuple)};
        []  -> {error, not_found};
        [_ | _] -> {error, more_than_one};
        Error -> Error
    end.

-spec read_all(dets:tab_name(), fun((tuple()) -> Obj)) -> list(Obj).
read_all(Table, Deserialize) ->
    Res = dets:select(Table,[{'_',[],['$_']}]),    
    lists:map(Deserialize, Res).

-spec put_account(#account{}) -> ok.
put_account(#account{account_number = AccountNumber, firstname = Firstname, surname = Surname, amount = Amount}) ->
    write(account, {AccountNumber, Firstname, Surname, Amount}).

deserialize_account({AccountNumber, Firstname, Surname, Amount}) ->
    #account{account_number = AccountNumber, firstname = Firstname, surname = Surname, amount = Amount}.

-spec get_account(account_number()) -> {ok, #account{}} | {error, any()}.
get_account(AccountNumber) ->
    read_one(account, AccountNumber, fun deserialize_account/1).

-spec get_all_accounts() -> list(#account{}).
get_all_accounts() -> read_all(account, fun deserialize_account/1).

-spec unique_account_number() -> unique_id().
unique_account_number() -> dets:update_counter(table_id, account, 1).

% holdover from Mnesia
-spec atomically(fun(() -> Ret)) -> Ret.
atomically(Fun) ->
    Fun().
