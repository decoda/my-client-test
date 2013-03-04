%% Author: tangj
%% Created: 2012-12-24
%% Description: TODO: Add description to lib_send
-module(lib_send).

%%
%% Include files
%%
-include("common.hrl").
%%
%% Exported Functions
%%
-export([send_to_socket/2,
		 send_to_sid/2,
		 send_to_all/1,
		 send_to_other/2]).

%%
%% API Functions
%%

send_to_socket(Socket, Bin) ->
	gen_tcp:send(Socket, Bin).

send_to_sid(PidSend, Bin) ->
	case is_pid(PidSend) of
		true ->
			PidSend ! {send, Bin};
		_ ->
			skip
	end.

send_to_all(Bin) ->
	F = fun(#user{pid_send = PidSend}, Acc) ->
				send_to_sid(PidSend, Bin),
				Acc
		end,
	ets:foldl(F, 0, ets_online).

send_to_other(SelfId, Bin) ->
	F = fun(#user{id = Id, pid_send = PidSend}, Acc) ->
				if Id =/= SelfId ->
					   send_to_sid(PidSend, Bin);
				   true ->
					   skip
				end,
				Acc
		end,
	ets:foldl(F, 0, ets_online).

%%
%% Local Functions
%%

