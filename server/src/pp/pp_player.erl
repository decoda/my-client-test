%% Author: tangj
%% Created: 2012-11-13
%% Description: TODO: Add description to pp_player
-module(pp_player).

%%
%% Include files
%%
-include("common.hrl").
%%
%% Exported Functions
%%
-export([handle/3]).

%%
%% API Functions
%%

handle(10002, Player, [X, Y]) ->
	UserId = Player#user.id,
	{ok, Bin} = pt_10:write(10002, [UserId, X, Y]),
	lib_send:send_to_all(Bin),
	{ok, Player#user{x = X, y = Y}};

handle(10003, Player, []) ->
	UserId = Player#user.id,
	F = fun(#user{id = Id} = User, Acc) ->
				if Id =/= UserId -> [User | Acc];
				   true -> Acc
				end
		end,
	UserList = ets:foldl(F, [], ets_online),
	{ok, Bin} = pt_10:write(10003, [UserList]),
	lib_send:send_to_sid(Player#user.pid_send, Bin),
	case UserList of
		[_ | _] ->
			{ok, Bin1} = pt_10:write(10003, [[Player]]),
			lib_send:send_to_other(UserId, Bin1);
		_ ->
			skip
	end,
	ok;

%% 错误处理
handle(_Cmd, _Player, Data) ->
	?PRINT("~p~n", [Data]),
    {error, "pp_player no match"}.

%%
%% Local Functions
%%

