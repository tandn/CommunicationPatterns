-module(sc_api).
-export([insert/2,
	lookup/1,
	delete/1]).

insert(Key, Value) ->
    case sc_store:lookup(Key) of
	{ok, Pid} ->
	    sc_element:replace(Pid,Value),
	    sc_event:replace(Key,Value);
	_ ->
	    {ok, Pid} = sc_element:create(Value),
	    sc_event:create(Key,Value),
	    sc_store:insert(Key,Pid)
    end.

lookup(Key) ->
    try
	{ok, Pid} = sc_store:lookup(Key),
	{ok, Value} = sc_element:get(Pid),
	sc_event:lookup(Key),
	{ok, Value}
    catch
	_:_ ->
	    {error,not_found}
    end.

delete(Key) ->
    case sc_store:lookup(Key) of
	{ok, Pid} ->
	    sc_element:delete(Pid),
	    sc_event:delete(Key);
	_ ->
	    ok
    end.
