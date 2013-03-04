%% Author: tangj
%% Created: 2012-11-1
%% Description: 启动文件
-module(main).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
		 server_start/1,
		 server_stop/1
		]).

%%
%% API Functions
%%

server_start(Apps) ->
	try
		ok = start_applications(Apps)
	after
		timer:sleep(100)
	end.

server_stop(Apps) ->
	ok = stop_applications(Apps).
	%% erlang:halt().

%%
%% Local Functions
%%

manage_applications(Iterate, Do, Undo, SkipError, ErrorTag, Apps) ->
	Iterate(fun (App, Acc) ->
					 case Do(App) of
						 ok -> [App | Acc];
						 {error, {SkipError, _}} -> Acc;
						 {error, Reason} ->
							 lists:foreach(Undo, Acc),
							 throw({error, {ErrorTag, App, Reason}})
					 end
			end, [], Apps),
	ok.

start_applications(Apps) ->
	manage_applications(fun lists:foldl/3,
						fun application:start/1,
						fun application:stop/1,
						already_started,
						cannot_start_application,
						Apps).

stop_applications(Apps) ->
	manage_applications(fun lists:foldr/3,
						fun application:stop/1,
						fun application:start/1,
						not_started,
						cannot_stop_application,
						Apps).