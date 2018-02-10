-module(crowd).
-export([test/0]).

%% crowd-source decision

test() ->
    crowd(5).

crowd(N) ->
    Pids = lists:map(fun(Id) ->
			     spawn(fun() -> volunteers(Id) end)
		     end, lists:seq(1,N)), % create 2*N members
    spawn(fun() -> initator(Pids) end),
    ok.

initator(L) ->
    Ret = multicall:multicall(L, decision, fun f/1, fun agg/1),
    io:format("Crowd Source Decision is ~p!~n",[Ret]).

f(Q) -> f(Q, 0, 0).

f([], _, _) -> false;
f(_, Y, _) when Y >= 3 -> true;
f(_, _, N) when N > 2 -> false;
f([H|T], Y, N) ->
    if H == yes ->
	    f(T, Y + 1, N);
       true ->
	    f(T, Y, N + 1)
    end.

agg(Q) -> agg(Q,0,0).

agg([],_,_) -> 0;
agg(_,Y,_) when Y >= 3 -> true;
agg(_,_,N) when N > 2 -> false;
agg([H|T],Y,N) when H == yes ->
    agg(T,Y+1,N);
agg([H|T],Y,N) when H == no ->
    agg(T,Y,N+1).


volunteers(Id) ->
    {A1,A2,A3} = os:timestamp(),
    random:seed(A1, A2, A3),
    D = random:uniform(10),
    receive
	{Pid, decision} ->
	    if D > Id ->
		    Pid ! yes;
	       true ->
		    Pid ! no
	    end
    end.
