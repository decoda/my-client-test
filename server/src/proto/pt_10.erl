%% Author: tangj
%% Created: 2012-12-21
%% Description: TODO: Add description to pt_10
-module(pt_10).

%%
%% Include files
%%
-include("common.hrl").
%%
%% Exported Functions
%%
-export([read/2, 
		 write/2]).

%%
%% API Functions
%%

read(10001, _) ->
	Id = mod_kernel:get_id(),
	io:format("player ~p login ~n", [Id]),
	{ok, login, Id};

read(10002, <<X:32, Y:32>>) ->
	{ok, [X, Y]};

read(10003, _) ->
	{ok, []};

read(10005, _Bin) ->
	{ok, []};

read(Cmd, _R) ->
	?WARNING_MSG("pt_10 read:~w", [Cmd]),
	{error, no_match}.

%% 登录
write(10001, [UserId, X, Y]) ->
	{ok, pt:pack(10001, <<UserId:64, X:32, Y:32>>)};

%% 路径
write(10002, [UserId, X, Y]) ->
	{ok, pt:pack(10002, <<UserId:64, X:32, Y:32>>)};

%% 加入场景
write(10003, [UserList]) ->
	F = fun(#user{id = Id, x = X, y = Y}) ->
				<<Id:64, X:32, Y:32>>
		end,
	Len = length(UserList),
	BinList = list_to_binary([F(X) || X <- UserList]),
	{ok, pt:pack(10003, <<Len:16, BinList/binary>>)};

%% 离开场景
write(10004, [UserId]) when is_integer(UserId) ->
	{ok, pt:pack(10004, <<1:16, UserId:64>>)};

write(Cmd, _R) ->
	?WARNING_MSG("pt_10,write:~w",[Cmd]),
	{ok, pt:pack(0, <<>>)}.

%%
%% Local Functions
%%

