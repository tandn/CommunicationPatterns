-module(join_continuations).
-export([test/0,fact/1,treeprod/1]).

test() ->
    true = (fact(5) == 120),
    true = (40320 == treeprod({{{1,2},{3,4}},{{5,6},{7,8}}})).


%% compute factorial
fact(N) when N >= 0 ->
    Pid = spawn(fun() -> fact() end),
    Pid ! {N, self()},
    receive
	Ret -> Ret
    end.

fact() ->
    receive
	{N, Cust} when N == 0 ->
	    Cust ! 1;
	{N, Cust} ->
	    C = spawn(fun() -> join_cont(Cust, N) end),
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
	    NewCust = spawn(fun() -> join_cont(Cust, 0, nil) end),
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
