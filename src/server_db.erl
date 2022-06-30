-module(server_db).
-export([start_link/1, handle_cast/2]).
-behaviour(gen_server).

start_link(Start) -> 
    gen_server:start_link({global, server_db}, ?MODULE, Start, [{debug, [trace]}]).

handle_cast({db_write, _Pid, {Table, Payload}}, _State) -> 
    database:write(Table, Payload),
    {noreply, _State};

handle_cast({db_read_one, Pid, {Table, Payload, Deserialize}}, _State) -> 
    Result = database:read_one(Table, Payload, Deserialize),
    gen_server:cast(Pid, Result),
    {noreply, _State};

handle_cast({db_read_all, Pid, {Table, Deserialize}}, _State) -> 
    Result = database:read_all(Table, Deserialize),
    gen_server:cast(Pid, Result),
    {noreply, _State}.

