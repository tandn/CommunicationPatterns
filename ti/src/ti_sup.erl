%%%-------------------------------------------------------------------
%% @doc ti top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ti_sup).

-behaviour(supervisor).

%% API
-export([start_link/1,start_child/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(LSock) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [LSock]).

start_child() ->
    supervisor:start_child(?SERVER,[]).
%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([LSock]) ->
    Child = {tcp_interface, {tcp_interface, start_link, [LSock]}, temporary, brutal_kill, worker, [tcp_interface]},
    {ok, { {simple_one_for_one, 0, 1}, [Child]} }.

%%====================================================================
%% Internal functions
%%====================================================================
