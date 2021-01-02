-module(multicall).
-export([multicall/4]).

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
