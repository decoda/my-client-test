%% Author: tangj
%% Created: 2012-12-21
%% Description: TODO: Add description to u
-module(u).

%%
%% Include files
%%
-include_lib("kernel/include/file.hrl").
%%
%% Exported Functions
%%
-export([c/0,
		 c/1,
		 u/0,
		 u/1,
		 admin/0]).

%%
%% API Functions
%%

c() ->
    c(5).
c(S) when is_integer(S) ->
	c:cd("../ebin"),
    case file:list_dir(".") of
        {ok, FileList} -> 
            Files = get_new_file(FileList, S * 60),
            info("---------check modules---------~n~w~n=========check modules=========", [Files]);
        Any -> info("Error Dir: ~w", [Any])
    end;
c([S]) when is_atom(S) ->
	S1 = tool:to_integer(tool:to_list(S)),
	case is_integer(S1) of
		true  ->
			c:cd("../ebin"),
    		case file:list_dir(".") of
				{ok, FileList} -> 
            		Files = get_new_file(FileList, S * 60),
            		info("---------check modules---------~n~w~n=========check modules=========", [Files]);
        		Any -> info("Error Dir: ~w", [Any])
    		end;
		_ ->
			info("ERROR======> Badarg ~p/~p ~n", [S, S1])
	end;
c(S) -> info("ERROR======> Badarg ~p ~n", [S]).

admin()->
    spawn(fun()->u(m) end),
    ok.

u() ->
    u(5).
u(m) ->
    StartTime = util:unixtime(),
    info("----------makes----------", []),
    c:cd("../"),
    make:all(),
    c:cd("ebin"),
    EndTime = util:unixtime(),
    Time = EndTime - StartTime,
    info("Make Time : ~w s", [Time]),
    u(Time / 60);
u(S) when is_number(S) ->
    case file:list_dir(".") of
        {ok, FileList} -> 
            Files = get_new_file(FileList, round(S * 60) + 3),
            info("---------modules---------~n~w~n----------nodes----------", [Files]),
            load(Files);
        Any -> info("Error Dir: ~w", [Any])
    end;
u(Files) when is_list(Files) ->
    info("---------modules---------~n~w~n----------nodes----------", [Files]),
    load(Files);
u(_) -> info("ERROR======> Badarg", []).

%% m(Files) when is_list(Files) ->
%%     StartTime = util:unixtime(),
%%     info("----------makes----------~n~w~n", [Files]),
%%     c:cd("../"),
%%     Res = make:files(Files, [debug_info,{i, "include"},{outdir, "ebin"}]),
%%     c:cd("ebin"),
%%     EndTime = util:unixtime(),
%%     Time = EndTime - StartTime,
%%     info("Make Time : ~w s", [Time]),
%%     Res.
%% 
%% info(V) ->
%%     info(V, []).
info(V, P) ->
    io:format(V ++ "~n", P).

get_new_file(Files, S) -> 
    get_new_file(Files, S, []).
get_new_file([], _S, Result) -> Result;
get_new_file([H | T], S, Result) ->
    NewResult = case string:tokens(H, ".") of
        [Left, Right] when Right =:= "beam" ->
            case file:read_file_info(H) of
                {ok, FileInfo} -> 
                    Now = calendar:local_time(),
                    case calendar:time_difference(FileInfo#file_info.mtime, Now) of
                        {Days, Times} -> 
                            Seconds = calendar:time_to_seconds(Times), 
                            case Days =:= 0 andalso Seconds < S of
                                true ->
                                    FileName = list_to_atom(Left),
                                    [FileName | Result];
                                false -> Result
                            end;
                        _ -> Result
                    end;
                _ -> Result
            end;
        _ -> Result
    end,
    get_new_file(T, S, NewResult).

load([]) -> ok;
load([FileName | T]) ->
    c:l(FileName),
    info("loaded: ~w", [FileName]),
    load(T).

%%
%% Local Functions
%%

