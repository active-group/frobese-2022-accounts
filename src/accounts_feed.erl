-module(accounts_feed).
-include("events.hrl").
-include("data.hrl").
-behaviour(gen_server).
-export([start_link/1, handle_cast/2, init/1, broadcast_new_account/1, register_test/1, create_accounts/0]).

start_link(Start) ->
    gen_server:start_link({local, account_feed}, ?MODULE, Start, [{debug, [trace]}]).

broadcast_new_account(Account) -> gen_server:cast(account_feed, Account).

send_events([], _) -> [];
send_events(EventsList, Pid) -> 
    lists:map(fun (Event) -> 
        gen_server:cast(Pid, {account_service, Event#event.number, create_account, Event#event.payload}) end, 
        EventsList
    ).

handle_cast({hallo, Pid, _Counter}, {RegisteredProcessesState, Count}) ->
    gen_server:cast(Pid, {selber_hallo}),
    {noreply, {RegisteredProcessesState, Count}};

handle_cast({register, Pid, Counter}, {RegisteredProcessesState, Count}) ->
    NewRegisteredProcessesState = sets:add_element(Pid, RegisteredProcessesState),
    AllEvents = events:get_events_from(Counter + 1),
    send_events(AllEvents, Pid),

    {noreply, {NewRegisteredProcessesState, Count}};

handle_cast(Account, {RegisteredProcessesState, Count}) -> 
    NewCount = Count + 1,
    RegisteredProcessesAsList = sets:to_list(RegisteredProcessesState),
    lists:map(fun (Pid) -> 
        gen_server:cast(Pid, {account_service, NewCount, create_account, Account}) end, 
        RegisteredProcessesAsList
    ),
    {noreply, {RegisteredProcessesState, NewCount}}.

register_test(Number) -> gen_server:cast(account_feed, {register, self(), Number}).


create_accounts() -> 
    NewAccount1 = #account{account_number = database:unique_account_number(), firstname = "Kelvin", surname = "Homann", amount = 1337},
    database:put_account(NewAccount1),
    accounts_feed:broadcast_new_account(NewAccount1),
    events:put_event(NewAccount1),
    NewAccount2 = #account{account_number = database:unique_account_number(), firstname = "Robin", surname = "Polke", amount = 1337},
    database:put_account(NewAccount2),
    accounts_feed:broadcast_new_account(NewAccount2),
    events:put_event(NewAccount2),
    NewAccount3 = #account{account_number = database:unique_account_number(), firstname = "Max", surname = "Mustermann", amount = 1337},
    database:put_account(NewAccount3),
    accounts_feed:broadcast_new_account(NewAccount3),
    events:put_event(NewAccount3),
    NewAccount4 = #account{account_number = database:unique_account_number(), firstname = "Mustermann", surname = "Max", amount = 1337},
    database:put_account(NewAccount4),
    accounts_feed:broadcast_new_account(NewAccount4),
    events:put_event(NewAccount4),
    NewAccount5 = #account{account_number = database:unique_account_number(), firstname = "Gustav", surname = "Gans", amount = 1337},
    database:put_account(NewAccount5),
    accounts_feed:broadcast_new_account(NewAccount5),
    events:put_event(NewAccount5),
    NewAccount6 = #account{account_number = database:unique_account_number(), firstname = "Peter", surname = "Lustig", amount = 1337},
    database:put_account(NewAccount6),
    accounts_feed:broadcast_new_account(NewAccount6),
    events:put_event(NewAccount6).

init(_) ->
    {ok, {sets:new(), 0}}.