%%% -------------------------------------------------------------------
%%% Author  : tangj
%%% Description :
%%%
%%% Created : 2012-11-1
%%% -------------------------------------------------------------------
-module(tcp_acceptor).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {sock, ref}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link(LSock) ->
    gen_server:start_link(?MODULE, {LSock}, []).

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
init({LSock}) ->
    gen_server:cast(self(), accept),
    {ok, #state{sock=LSock}}.

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
handle_call(Request, _From, State) ->
	io:format("tcp_acceptor call not match ~p~n", [Request]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(accept, State) ->
    {noreply, NewState} = accept(State),
	{noreply, NewState};

handle_cast(Msg, State) ->
	io:format("tcp_acceptor cast not match ~p~n", [Msg]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({inet_async, LSock, Ref, {ok, Sock}},
            State = #state{sock=LSock, ref=Ref}) ->
	%io:format("inet_async ok ~p~n", [LSock]),
	case set_sockopt(LSock, Sock) of
		ok -> ok;
		{error, Reason} -> exit({set_sockopt, Reason})
	end,
	
	%% 开启客户端reader进程,并保持监听进程不变
	{ok, Child} = supervisor:start_child(tcp_client_sup, []),
	ok = gen_tcp:controlling_process(Sock, Child),
	Child ! {go, Sock},

	%% Returns the address and port for the other end of a connection
	%{ok, Client} = inet:peername(Sock),
	%io:format("client connect~p~nread pid ~p socket ~p~n", [Client, Child, Sock]),
	
	%% accept more
    accept(State);
	
handle_info({inet_async, LSock, Ref, {error, closed}},
            State=#state{sock=LSock, ref=Ref}) ->
    %% It would be wrong to attempt to restart the acceptor when we
    %% know this will fail.
    io:format("inet_async error ~p~n", [LSock]),
    {stop, normal, State};

handle_info({inet_async, LSock, Ref, {error, Reason}},
            State=#state{sock=LSock, ref=Ref}) ->
    {stop, {accept_failed, Reason}, State};

handle_info(Info, State) ->
	io:format("tcp_acceptor info not match ~p~n", [Info]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
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
%% 异步accept
accept(State = #state{sock=LSock}) ->
    case prim_inet:async_accept(LSock, -1) of
        {ok, Ref} -> {noreply, State#state{ref=Ref}};
        Error     -> {stop, {cannot_accept, Error}, State}
    end.

set_sockopt(LSock, Sock) ->
	%% patch up the socket so it looks like one we got from
    %% gen_tcp:accept/1
    %%{ok, _Mod} = inet_db:lookup_socket(LSock),
    %% inet_db:register_socket(Sock, Mod),
	%% Mod =:= inet_tcp
	true = inet_db:register_socket(Sock, inet_tcp),
	case prim_inet:getopts(LSock, [active, nodelay, keepalive]) of
		{ok, Opts} ->
			case prim_inet:setopts(Sock, Opts) of
				ok    -> ok;
				Error -> 
					catch erlang:port_close(Sock),
					Error
			end;
		Error ->
			catch erlang:port_close(Sock),
			Error
	end.