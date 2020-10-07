%% @author Alessandro
%% @doc @todo Add description to graph_utils.


-module(graph_utils).
-import('utilities', [print_debug_message/1, print_debug_message/2, print_debug_message/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([getWeight/3, getDataPath/2]).



%% ====================================================================
%% Internal functions
%% ====================================================================

%% From NODE with ID1 to ID2
getWeight(ID_1, ID_2, []) -> 
	if  ID_1 == ID_2 -> 0;
		true -> -1
	end;

getWeight(ID_1, ID_2, [H | T]) ->
	{{V1, V2}, W} = H,
	
	if  ID_1 == ID_2 -> 0;
	    (((V1 == ID_1) and (V2 == ID_2)) or ((V1 == ID_2) and (V2 == ID_1))) -> W;
		true -> getWeight(ID_1, ID_2, T)
	end.

getDataPath([], _Target_ID) -> {-1, {-1, []}};
getDataPath([H | T], Target_ID) -> 
	{ID, {Cost, Points}} = H,
	if ID == Target_ID -> {Cost, Points};
	   true -> getDataPath(T, Target_ID) 
	end.
