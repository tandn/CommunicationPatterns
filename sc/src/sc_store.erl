-module(sc_store).
-export([init/0,
	 insert/2,
	 lookup/1,
	 delete/1,
	 pid_to_key/1]).

-define(DEFAULT_WAIT_TIME, 2000).

-record(key_to_pid,{key,pid}).

init() ->
    ok = ensure_contact(),
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:start(),

    rd:add_target(simple_cache),
    rd:add_local(simple_cache,node()),
    rd:trade(),
    timer:sleep(?DEFAULT_WAIT_TIME), % trade is asynchronous : TODO: change to synchronous with timeout
    {ok, CachedNodes} = rd:fetch(simple_cache),
    io:format("cached nodes found: ~p~n",[CachedNodes]),
    db_dynamic_replicate(CachedNodes).

insert(Key,Pid) ->
    mnesia:dirty_write(key_to_pid,#key_to_pid{key=Key, pid=Pid}),
    ok.

lookup(Key) ->
    case mnesia:dirty_read(key_to_pid,Key) of
	[{key_to_pid,Key,Pid}] -> {ok, Pid};
	[] -> {error,not_found}
    end.

delete(Pid) ->
    case mnesia:dirty_index_read(key_to_pid, Pid, #key_to_pid.pid) of
	[#key_to_pid{} = Record] ->
	    mnesia:dirty_delete_object(Record);
	_ ->
	    ok
    end.

pid_to_key(Pid) ->
    case mnesia:dirty_index_read(key_to_pid, Pid, #key_to_pid.pid) of
	[key_to_pid,Key,Pid] ->
	    {ok, Key};
	_ ->
	    {error, not_found}
    end.

db_dynamic_replicate([]) ->
    mnesia:create_table(key_to_pid,[{index,[pid]},{attributes,record_info(fields,key_to_pid)}]);
db_dynamic_replicate([Node|T]) ->
    case mnesia:change_config(extra_db_nodes,[Node]) of
	{ok,[Node]} ->
	    mnesia:add_table_copy(schema, node(), ram_copies),
	    mnesia:add_table_copy(key_to_pid, node(), ram_copies),
	    Tables = mnesia:system_info(tables),
	    WaitTime = get_env(sc,wait_table_time,?DEFAULT_WAIT_TIME),
	    mnesia:wait_for_tables(Tables,WaitTime),
	    ok;
	_ ->
	    db_dynamic_replicate(T)
    end.


get_env(App, Key, Default) ->
    case application:get_env(App, Key) of
	undefined ->
	    Default;
	{ok, Value} ->
	    Value
    end.

ensure_contact() ->
    ContactNodes = get_env(sc, contact_nodes, []),
    case ContactNodes of
	[] -> {error,no_contact_node};
	_ -> ensure_contact(ContactNodes)
    end.

ensure_contact(ContactNodes) ->
    A = [X || X <- ContactNodes, net_adm:ping(X) =:= pong],
    case A of
	[] -> {error,contact_node_unreachable};
	_ ->
	    ContactTime = get_env(sc, wait_contact_time, ?DEFAULT_WAIT_TIME),
	    SliceTime = round(ContactTime / 10),
	    wait(length(A),SliceTime,10)
    end.

wait(_, _, 0) ->
    ok;
wait(NumNodes, SliceTime, Iter) ->
    case length(nodes()) > NumNodes of
	true ->
	    ok;
	false ->
	    timer:sleep(SliceTime),
	    wait(NumNodes,SliceTime,Iter-1)
    end.
