%%%-------------------------------------------------------------------
%% @doc sc top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ElementSup = {sc_element_sup, {sc_element_sup, start_link, []}, permanent, 2000, supervisor, [sc_element_sup]},
    EventManager = {sc_event, {sc_event, start_link, []}, permanent, 2000, worker, [sc_event]},
    {ok, { {one_for_one, 4, 1000}, [ElementSup, EventManager]} }.

%%====================================================================
%% Internal functions
%%====================================================================
