%%% -------------------------------------------------------------------
%%% Author  : tangj
%%% Description :
%%%
%%% Created : 2012-12-24
%%% -------------------------------------------------------------------
-module(mod_kernel).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("common.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start_link/0,
		 get_mod_kernel/0,
		 get_id/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {id = 0}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

get_mod_kernel() ->
	case tool:whereis_name({local, ?MODULE}) of
		Pid when is_pid(Pid) -> Pid;
		_ -> start_mod_kernel()
	end.

start_mod_kernel() ->
	case supervisor:start_child(
	  server_sup, {mod_kernel,
				   {mod_kernel, start_link,[]},
				   permanent, 10000, worker, [mod_kernel]}) of
		{ok, Pid} ->
			Pid;
		_ ->
			undefined
	end.

get_id() ->
	Pid = get_mod_kernel(),
	gen_server:call(Pid, {get_id}).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	init_ets(),
    {ok, #state{id = 0}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(Request, From, State) ->
	try
		do_call(Request, From, State)
	catch
		_:_Reason -> {reply, ok, State}
	end.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
	try
		do_cast(Msg, State)
	catch
		_:_Reason ->  {noreply, State}
	end.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
	try
		do_info(Info, State)
	catch
		_:_Reason ->  {noreply, State}
	end.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
do_call({get_id}, _From, State) ->
	Id = State#state.id + 1,
	{reply, Id, State#state{id = Id}}.
    
do_cast(_Msg, State) ->
    {noreply, State}.
    
do_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
init_ets() ->
	ets:new(ets_online, [{keypos,#user.id}, named_table, public, set]), %%本节点在线用户列表
	ok.