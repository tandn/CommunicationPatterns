%%%-------------------------------------------------------------------
%% @doc ti public API
%% @end
%%%-------------------------------------------------------------------

-module(ti_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(DEFAULT_PORT,1055).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Port = case application:get_env(ti,port) of
	       undefined -> ?DEFAULT_PORT;
	       {ok, P} -> P
	   end,
    {ok, LSock} = gen_tcp:listen(Port,[{active,true},{reuseaddr, true}]),
    case ti_sup:start_link(LSock) of
	{ok, Pid} ->
	    ti_sup:start_child(),
	    {ok, Pid};
	_ ->
	    error
    end.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
