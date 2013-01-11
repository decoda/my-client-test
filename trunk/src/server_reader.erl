%% Author: tangj
%% Created: 2012-11-1
%% Description: TODO: Add description to server_reader
-module(server_reader).

%%
%% Include files
%%
-include("common.hrl").
%%
%% Exported Functions
%%
-export([
		 start_link/0,
		 init/0
		]).


-define(HEART_TIMEOUT, 5 * 60 * 1000). 
%% 消息头长度，async_recv需等到这么多才会开始接受数据
-define(HEADER_LENGTH, 5).


-record(client, {player_pid = undefined}).

%%
%% API Functions
%%

start_link() ->
    {ok, proc_lib:spawn_link(?MODULE, init, [])}.

init() ->
	Client = #client{player_pid = undefined},
	receive
		{go, Socket} ->
			try
				get_socket(Socket, Client)
			catch
				_Type:Reason ->
					%%?PRINT("get_socket exception ~p~n", [Reason]),
					client_lost(Socket, {error, Reason}),
					error
			end;
		_ ->
			skip
	end.

%%
%% Local Functions
%%

%% 异步获取socket的数据
get_socket(Socket, Client) ->
	Ref = async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT),
	receive
		{inet_async, Socket, Ref, {ok, <<Len:16, IsZip:8, Cmd:16>>}} ->
			case get_socket_other(Socket, Len - ?HEADER_LENGTH, IsZip) of
				{ok, BinData} ->
					case pt:routing(Cmd, BinData) of
						{ok, login, UserId} when is_integer(UserId) ->
							{ok, Pid} = mod_player:start(UserId, Socket),
							gen_server:cast(Pid, {enter_server}),
							NewClient = Client#client{player_pid = Pid},
							get_socket_data(Socket, NewClient);
						{ok, heart} ->
							get_socket(Socket, Client);
						Unknow ->
							client_lost(Socket, {error, Unknow})
					end;
				{false, Other1} ->
					io:format("~p~n", [Other1]),
					client_lost(Socket, {error, Other1})
			end;
		{inet_async, Socket, Ref, {error,timeout}} ->
			io:format("socket ~p timeout ~n", [Socket]),
			client_lost(Socket, {error,timeout});
		{inet_async, Socket, Ref, {error,closed}} ->
			io:format("socket ~p closed ~n", [Socket]),
			erlang:port_close(Socket);
		Other ->
			io:format("async_recv Other : ~p ~n", [Other]),
			client_lost(Socket, Other)
	end.

%% 登录游戏后的消息接收处理
get_socket_data(Socket, #client{player_pid = Pid} = Client) ->
	Ref = async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT),
	receive
		{inet_async, Socket, Ref, {ok, <<Len:16, IsZip:8, Cmd:16>>}} ->
			case get_socket_other(Socket, Len - ?HEADER_LENGTH, IsZip) of
				{ok, BinData} ->
					case pt:routing(Cmd, BinData) of
						{ok, heart} ->
							skip;
						{ok, Bin} ->
							gen_server:cast(Pid, {socket_event, Cmd, Bin});		
						_ ->
							skip
					end,
					get_socket_data(Socket, Client);
				{false, Other1} ->
					io:format("~p~n", [Other1]),
					client_lost(Socket, Pid, {error, Other1})
			end;
		{inet_async, Socket, Ref, {error,timeout}} ->
			io:format("socket ~p timeout ~n", [Socket]),
			client_lost(Socket, Pid, {error,timeout});
		{inet_async, Socket, Ref, {error,closed}} ->
			io:format("socket ~p closed ~n", [Socket]),
			client_lost(Socket, Pid, {error,closed});
		Other ->
			io:format("async_recv Other : ~p ~n", [Other]),
			client_lost(Socket, Pid, Other)
	end.

%% 获取除包头的数据
get_socket_other(Socket, BodyLen, IsZip) ->
	if BodyLen > 0 ->
		   Ref = async_recv(Socket, BodyLen, ?HEART_TIMEOUT),
		   receive
			   {inet_async, Socket, Ref, {ok, Binary}} ->
				   case IsZip of
					   0 ->
						   {ok,Binary};
					   1 ->
						   UnBinary = zlib:uncompress(Binary),
						   {ok, UnBinary}
				   end;
			   Other ->
				   {false, Other}
		   end;
	   true ->
		   {ok, <<>>}
	end.   

%% 客户端掉线
client_lost(Socket, Reason) ->
	timer:sleep(100),
	catch erlang:port_close(Socket),
    exit({client_lost, Reason}).

%% 登录后客户端掉线
client_lost(Socket, Pid, Reason) ->
	mod_player:stop(Pid, Reason),
	catch erlang:port_close(Socket),
	exit({client_lost, Reason}).

%% 接收数据
async_recv(Sock, Length, Timeout) when is_port(Sock) ->
    case prim_inet:async_recv(Sock, Length, Timeout) of
        {error, Reason} -> throw({Reason});
		{ok, Res}		-> Res;
		Res 			-> Res
    end.