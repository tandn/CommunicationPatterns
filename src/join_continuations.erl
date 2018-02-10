-module(join_continuations).
-compile([fact/1,treeprod/1]).

%% compute factorial
fact(N) when N >= 0 ->
    Pid = spawn(fun fact/0),
    Pid ! {N, self()},
    receive
	Ret -> Ret
    end.

fact() ->
    receive
	{N, Cust} when N == 0 ->
	    Cust ! 1;
	{N, Cust} ->
	    C = spawn(fun join_cont/2 [Cust, N]),
	    self() ! {N - 1, C}
    end,
    fact().


join_cont(Cust, N) ->
    receive
	K ->
	    Cust ! N * K
    end.



%% compute tree product
treeprod(T) ->
    Pid = spawn(fun treeprod/0),
    Pid ! {T, self()},
    receive
	Ret ->
	    Ret
    end.

treeprod() ->
    receive
	{T, Cust} when is_integer(T) ->
	    Cust ! T;
	{T, Cust} ->
	    NewCust = spawn(fun join_cont/3 [Cust, 0, nil]),
	    self() ! {left(T), NewCust},
	    self() ! {right(T), NewCust}
    end,
    treeprod().

join_cont(Cust, NArgs, First) ->
    receive
	Num when NArgs == 0 ->
	    join_cont(Cust, 1, Num);
	Num ->
	    Cust ! First*Num
    end.

left({L,_}) ->
    L.
right({_,R}) ->
    R.
