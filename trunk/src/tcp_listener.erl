%%% -------------------------------------------------------------------
%%% Author  : tangj
%%% Description :监听
%%%
%%% Created : 2012-11-1
%%% -------------------------------------------------------------------
-module(tcp_listener).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% -record(state, {}).

-define(TCP_OPTIONS, [binary, 
					  {packet, 0}, 
					  {active, false}, 
					  {reuseaddr, true}, 
					  {nodelay, false}, 
					  {delay_send, true}, 
					  {send_timeout, 5000}, 
					  {keepalive, true}, 
					  {exit_on_close, true}]).

%% ====================================================================
%% External functions
%% ====================================================================

start_link(Port) ->
    gen_server:start_link(?MODULE, {Port}, []).

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
init({Port}) ->
	process_flag(trap_exit, true),
	case gen_tcp:listen(Port, ?TCP_OPTIONS) of
		{ok, LSock} ->
			lists:foreach(fun (_) ->
								   {ok, _APid} = supervisor:start_child(
												   tcp_acceptor_sup, [LSock])
						  end,
						  lists:duplicate(10, dummy)),
			{ok, LSock};
		{error, Reason} ->
			{stop, {cannot_listen, Port, Reason}}
	end.

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
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, State) ->
	{ok, {IPAddress, Port}} = inet:sockname(State),
	gen_tcp:close(State),
	io:format("ip ~p stopped on ~p ~n", [IPAddress, Port]),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

