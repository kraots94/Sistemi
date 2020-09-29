-module(my_util).
-compile(export_all).

%print text e variabile
println(String,Var) ->
	io:format(String ++ " ~p~n" , [Var]).

%print normal text
println(String) ->
	io:format(String ++ "~n", []).

%print list
printList(String,List) -> 
	io:format(String ++ " ~w~n" , [List]).

