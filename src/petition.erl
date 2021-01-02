-module(petition).
-export([test/0]).
test() ->
    petition(5). % threshold is 5 signatures

%% petition scenario
petition(N) ->
    Pids = lists:map(fun(Id) ->
			     spawn(fun() -> members(Id) end)
		     end, lists:seq(1,N*2)), % create 2*N members
    spawn(fun() -> initator(Pids, N) end),
    ok.

initator(L, N) ->
    Ret = multicall:multicall(L, petition, f(N), fun agg/1),
    io:format("Petition Vote is ~p!~n",[Ret]).

f(N) ->
    fun(Q) -> f(Q, 0, N) end.

f([], _, _) -> false;
f([_|T], Total, N) ->
    if Total > N ->
	    true;
       true ->
	    f(T, Total + 1, N)
    end.

agg([]) -> 0;
agg([_|T]) ->
    1 + agg(T).


members(Id) ->
    {A1,A2,A3} = os:timestamp(),
    random:seed(A1, A2, A3),
    D = random:uniform(10),
    receive
	{Pid, petition} ->
	    if D > Id ->
		    Pid ! yes;
	       true ->
		    ok
	    end
    end.
