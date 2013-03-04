%% Author: tangj
%% Created: 2012-11-1
%% Description: server应用app
-module(server_app).

-behaviour(application).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
	 start/2,
	 stop/1
        ]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------


%% ====================================================================!
%% External functions
%% ====================================================================!
%% --------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start(_Type, _StartArgs) ->
	{ok, SupPid} = server_sup:start_link(),
	
	loglevel:set(6),
	mod_kernel:get_mod_kernel(),

	ListenPort = tool:get_config(myserver, port),
	ListenIp = tool:get_config(myserver, ip),
	
	io:format("node ~p server_app~p ~nserver start ~p:~p~n", 
			  [node(), self(), ListenIp, ListenPort]),
	
	ok = start_tcp(ListenPort),
	ok = start_client(),
	
	{ok, SupPid}.

%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% 监听accpet
start_tcp(Port) ->
	{ok, _} = supervisor:start_child(
				server_sup,
				{tcp_listener_sup,
				 {tcp_listener_sup, start_link, [Port]},
				 transient, infinity, supervisor, [tcp_listener_sup]}),
	ok.

%% 处理客户端连接
start_client() ->
	{ok,_} = supervisor:start_child(
			   server_sup,
			   {tcp_client_sup,
				{tcp_client_sup, start_link,[]},
				transient, infinity, supervisor, [tcp_client_sup]}),
	ok.
