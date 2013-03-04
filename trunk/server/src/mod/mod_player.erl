%%% -------------------------------------------------------------------
%%% Author  : tangj
%%% Description :
%%%
%%% Created : 2012-11-2
%%% -------------------------------------------------------------------
-module(mod_player).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("common.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/2,
		 stop/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(mod_player_state, {player = #user{},
						   socket = undefined}).

%% ====================================================================
%% External functions
%% ====================================================================

start(UserId, Socket) ->
	gen_server:start(?MODULE, [UserId, Socket], []).

stop(Pid, Reason) ->
	gen_server:cast(Pid, {stop, Reason}).

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
init([UserId, Socket]) ->
	Pid = self(),
	PidSend = spawn_link(fun() -> send_msg(Socket, Pid) end),
	User = #user{id = UserId,
				 pid = Pid,
				 pid_send = PidSend},
	ets:insert(ets_online, User),
    {ok, #mod_player_state{player = User,
						   socket = Socket}}.

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
		_:Reason ->
			?WARNING_MSG("mod_player handle_call is exception:~w~n,Request:~w",[Reason, Request]),
			?WARNING_MSG("get_stacktrace:~p",[erlang:get_stacktrace()]),
			{reply, ok, State}
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
		_:Reason ->
			?WARNING_MSG("mod_player handle_cast is exception:~w~n,Msg:~w",[Reason, Msg]),
			?WARNING_MSG("get_stacktrace:~p",[erlang:get_stacktrace()]),
			{noreply, State}
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
		_:Reason ->
			?WARNING_MSG("mod_player handle_info is exception:~w~n,Info:~w",[Reason, Info]),
			?WARNING_MSG("get_stacktrace:~p",[erlang:get_stacktrace()]),
			{noreply, State}
	end.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, State) ->
	Player = State#mod_player_state.player,
	{ok, Bin} = pt_10:write(10004, [Player#user.id]),
	lib_send:send_to_other(Player#user.id, Bin),
	ets:delete(ets_online, State#mod_player_state.player#user.id),
	catch erlang:port_close(State#mod_player_state.socket),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
do_call(Request, _From, State) ->
	{reply, ok, State}.
    
do_cast({socket_event, Cmd, Bin}, State) ->
	OldPlayer = State#mod_player_state.player,
	case routing(Cmd, OldPlayer, Bin) of
		{error, Reason} ->
			NewPlayer = OldPlayer,
			?PRINT("socket event error ~p~n", [Reason]);
		{ok, Player} ->
			ets:insert(ets_online, Player),
			NewPlayer = Player;
		_ ->
			NewPlayer = OldPlayer,
			skip
	end,
	{noreply, State#mod_player_state{player = NewPlayer}};

do_cast({enter_server}, State) ->
	Player = State#mod_player_state.player,
	{ok, Bin} = pt_10:write(10001, [Player#user.id, Player#user.x, Player#user.y]),
	lib_send:send_to_sid(Player#user.pid_send, Bin),
	{noreply, State};

do_cast({stop, _Reason}, State) ->
	{stop, normal, State}.
    
do_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
routing(Cmd, Player, Bin) ->
	[H1, H2, _, _, _] = integer_to_list(Cmd),
	case [H1, H2] of
		"10" -> pp_player:handle(Cmd, Player, Bin);
		_ ->  {error, "player routing failed"}
	end.

%% 发消息
send_msg(Socket, PlayerPid) ->
	receive
		{send, Bin} ->		
			catch erlang:port_command(Socket, Bin, [force]),			  
			send_msg(Socket, PlayerPid);
		{inet_reply, _Sock, ok} ->
			send_msg(Socket, PlayerPid);
		{inet_reply, _Sock, _Result} ->
			stop(PlayerPid, "socket is error"),
			stop;
		{stop} ->
			stop;
		Error ->
			?WARNING_MSG("Error:~w", [Error]),
			send_msg(Socket, PlayerPid)
	end.