%%%-------------------------------------------------------------------
%%% @author tan duong <tanduong@localhost>
%%% @copyright (C) 2020, tan duong
%%% @doc
%%%
%%% @end
%%% Created : 19 Dec 2020 by tan duong <tanduong@localhost>
%%%-------------------------------------------------------------------
-module(rd).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([add_target/1,
	 add_local/2,
	 fetch/1,
	 trade/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {target_types, % wanted resource types
	       local_tuples,  % owned resrouces
	       found_tuples}). % cached resources that matches the wanted resources

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


add_target(Type) ->
    gen_server:cast(?SERVER, {add_target, Type}).

add_local(Type, Instance) ->
    gen_server:cast(?SERVER, {add_local, {Type, norm_instance(Instance)}}).

fetch(Type) ->
    gen_server:call(?SERVER, {fetch, Type}).

trade() ->
    gen_server:cast(?SERVER, trade).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{target_types = [], % wanted resource types
	       local_tuples = dict:new(),  % owned resrouces
	       found_tuples = dict:new()}}. %cached resources that matches the wanted resources

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({fetch, Type}, _From, State=#state{found_tuples=FoundResources}) ->
    Reply = case dict:find(Type, FoundResources) of
		error -> {ok, []};
	        Other -> Other
	    end,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({add_target, Type}, State=#state{target_types = Types}) ->
    NewTypes=[Type | lists:delete(Type,Types)],
    {noreply, State#state{target_types=NewTypes}};
handle_cast({add_local, {Type, Instance}}, State=#state{local_tuples = Local}) ->
    NewLocal = add_resource(Type,Instance,Local),
    {noreply, State#state{local_tuples=NewLocal}};
handle_cast(trade, State=#state{local_tuples=Local}) ->
    lists:foreach(fun(Node) ->
			  gen_server:cast({?SERVER,Node},{trade, {node(), Local}})
		  end,
		  nodes()),
    {noreply,State};
handle_cast({trade, {ReplyTo, Remote}}, State=#state{target_types=Types,
							     local_tuples=Local,
							     found_tuples=Found}) ->
    Filtered = filter_resources(Types,Remote),
    NewFound = add_resources(Filtered,Found),
    case ReplyTo of
	noreply ->
	    ok;
	_ ->
	    gen_server:cast({?SERVER,ReplyTo}, {trade,{noreply, Local}})
    end,
    {noreply,State#state{found_tuples=NewFound}}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

norm_instance([]) ->
    [];
norm_instance([H|T]) ->
    [H | norm_instance([X || X <- T, X =/= H])];
norm_instance(X) ->
    [X].

add_resource(Type, Instance, Found) ->
    case dict:find(Type,Found) of
	{ok, ResourceList} ->
	    dict:store(Type, norm_instance(Instance ++ lists:delete(Type, ResourceList)), Found);
	error ->
	    dict:store(Type, Instance, Found)
    end.

add_resources([], Acc) ->
    Acc;
add_resources([{Type, Instance} | Rest], Found) ->
    add_resources(Rest, add_resource(Type,Instance,Found)).


%% types: [ti,tj,tk]
%% remote: dict of [t1, []], [t2,[]]
%% return list of [ti, []], [tj,[]]
filter_resources(Types, RemoteResources) ->
    F = fun(Type, Acc) ->
	   case dict:find(Type, RemoteResources) of
	       {ok, ResourceList} ->
		   [{Type,ResourceList} | Acc];
	       error ->
		   Acc
	   end
	end,
    lists:foldl(F, [], Types).
