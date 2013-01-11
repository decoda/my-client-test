%% Author: tangj
%% Created: 2012-11-2
%% Description: TODO: Add description to tool
-module(tool).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
		 get_config/2,
		 for/3,
		 whereis_name/1,
		 register/3
		 ]).

%%
%% API Functions
%%

%% 获取应用配置
get_config(App, Key) ->
    case application:get_env(App, Key) of
 	{ok, false} -> throw(undefined);
	{ok, Value} -> Value;
 	undefined 	-> throw(undefined)
    end.

%% for循环
for(Max, Max, Fun) -> Fun(Max);
for(Min, Max, Fun) -> Fun(Min), for(Min + 1, Max, Fun).

whereis_name({local, Atom}) -> 
	erlang:whereis(Atom);

whereis_name({global, Atom}) ->
	global:whereis_name(Atom).
 
register(local, Name, Pid) ->
	erlang:register(Name, Pid);

register(global, Name, Pid) ->
	global:re_register_name(Name, Pid).


%%
%% Local Functions
%%

