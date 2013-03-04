%% Author: tangj
%% Created: 2012-11-13
%% Description: TODO: Add description to pt
-module(pt).

%%
%% Include files
%%

-define(MSG_HEART, 1).			%% 心跳消息协议号
-define(MSG_PLAYER_LOGIN, 10).	%% 进入服务器协议号

%%
%% Exported Functions
%%
-export([read_string/1,
		 write_string/1,
		 pack/2,
		 routing/2]).

%%
%% API Functions
%%


%% 消息处理
routing(?MSG_HEART, _) -> 
	%%io:format("heart msg ~n"), 
	{ok, heart};
routing(Cmd, Binary) ->
	%%取前面二位区分功能类型 
	[H1, H2, _, _, _] = integer_to_list(Cmd),
	Module = list_to_atom("pt_"++[H1,H2]),
    Module:read(Cmd, Binary).

%% 读取字符串
read_string(Bin) ->
    case Bin of
        <<Len:16, Bin1/binary>> ->
            case Bin1 of
                <<Str:Len/binary-unit:8, Rest/binary>> ->
                    {binary_to_list(Str), Rest};
                _R1 ->
                    {[],<<>>}
            end;
        _R1 ->
            {[],<<>>}
    end.

%% 写入字符串
write_string(Str) when is_list(Str) ->
	Bin = list_to_binary(Str),
    Len = byte_size(Bin),	
	<<Len:16, Bin/binary>>.

%% 打包信息，添加消息头
pack(Cmd, Data) ->
	Len = byte_size(Data) + 5,
	IsZip = 0,
	<<Len:16, IsZip:8, Cmd:16, Data/binary>>.

%%
%% Local Functions
%%

