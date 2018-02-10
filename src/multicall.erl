-module(multicall).
-export([test/0]).

test() ->
    petition(10). % threshold is 5 signatures

petition(N) ->
    Pids = lists:map(fun(Id) ->
			     spawn(fun() -> members(Id) end)
		     end, lists:seq(1,N*2)), % create 2*N members
    spawn(fun() -> initator(Pids, N) end),
    ok.

initator(L, N) ->
    Ret = multicall(L, petition, f(N), fun agg/1),
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
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    D = random:uniform(10),
    receive
	{Pid, petition} ->
	    if D < 6 ->
		    Pid ! yes;
	       true ->
		    ok
	    end
    end.


multicall(L, M, C, F) ->
    S = self(),
    Pid = spawn(fun() -> faciliator(S, L, M, C, F) end),
    receive
	{Pid, Ret} ->
	    Ret
    after 2000 ->
	    0
    end.

faciliator(P, L, M, C, F) ->
    S = self(),
    multicast({S, M}, L),
    gather(P, S, C, F, []).

gather(P, S, C, F, Acc) ->
    receive
	Msg ->
	    NewAcc = [Msg | Acc],
	    case C(NewAcc) of
		true ->
		    P ! {S, F(NewAcc)};
		false ->
		    gather(P, S, C, F, NewAcc)
	    end
    end.

multicast(Msg, L) ->
    [Pid ! Msg || Pid <- L].
