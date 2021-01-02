%%%-------------------------------------------------------------------
%% @doc sc_element top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sc_element_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_element/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_element(Value, LeaseTime) ->
    supervisor:start_child(?SERVER, [Value, LeaseTime]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Child = {sc_element, {sc_element, start_link, []}, temporary, brutal_kill, worker, [sc_element]},
    {ok, { {simple_one_for_one, 0, 1}, [Child]} }.

%%====================================================================
%% Internal functions
%%====================================================================
