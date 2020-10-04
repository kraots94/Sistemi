-module(my_util).
-import(lists,[reverse/1]). 
-import(math, [pow/2]).
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

init_random_generator() ->
	println("Initiating random generatior.~n"),
	{T_M, T_S, T_m} = erlang:timestamp(),
	io:format("Current Timestamp: ~w ~w ~w ~n", [T_M, T_S, T_m]),
	rand:seed(exro928ss, {T_M, T_S, T_m}).

generate_random_number(MAX) ->
	N = rand:uniform(MAX),
%	io:format("Generated number: ~w ~n", [N]),
	N.

createPairs([A , B | []], ACC) -> 
	NEW_LIST = ACC ++ [{A, B}],
	NEW_LIST;

createPairs([A | []], _ACC) ->
	[{A,A}];

createPairs([A , B | Tail], ACC) ->
	NEW_LIST = ACC ++ [{A, B}],
	createPairs([B] ++ Tail, NEW_LIST).

arraySum(_A) -> 0.