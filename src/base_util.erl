%% @author Alessandro
%% @doc @todo Add description to base_util.


-module(base_util).
-import(lists,[reverse/1]). 
-import(math, [pow/2]).
-import('utilities', [print_debug_message/1, print_debug_message/2, print_debug_message/3]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([base26to10/1]).


base26to10(N) -> 
	Res = trunc(base26to10calc(reverse(N), 0)),
	Res.

%% ====================================================================
%% Internal functions
%% ====================================================================


base26to10calc([], _Acc) -> 0;
base26to10calc([H | T], Acc) -> 
	Val = H - 97,
	Res = (pow(26, Acc) * Val) + base26to10calc(T, Acc+1),
	Res.