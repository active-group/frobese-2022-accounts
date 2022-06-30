-module(accounts_feed).
-include("data.hrl").
-behaviour(gen_server).
-export([start_link/1, handle_cast/2, init/1, broadcast_new_account/1, register_test/0]).

start_link(Start) ->
    gen_server:start_link({local, account_feed}, ?MODULE, Start, [{debug, [trace]}]).

broadcast_new_account(Account) -> gen_server:cast(account_feed, Account).

handle_cast({register, Pid, Counter}, {RegisteredProcessesState, _}) ->
    NewRegisteredProcessesState = sets:add_element({Pid, Counter}, RegisteredProcessesState),
    {noreply, {NewRegisteredProcessesState, 0}};

handle_cast(Account, {RegisteredProcessesState, Count}) -> 
    NewCount = Count + 1,
    RegisteredProcessesAsList = sets:to_list(RegisteredProcessesState),
    lists:map(fun ({Pid, _}) -> 
        gen_server:cast(Pid, {account_service, NewCount, create_account, Account}) end, 
        RegisteredProcessesAsList
    ),
    {noreply, {RegisteredProcessesState, NewCount}}.
    
register_test() -> gen_server:cast(account_feed, {register, self(), 0}).

init(_) ->
    {ok, {sets:new(), 0}}.