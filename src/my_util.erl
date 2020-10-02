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

generate_random_number() ->
	N = rand:uniform(100),
	io:format("Generated number: ~w ~n", [N]),
	N.

% C è una lettera nella forma di lista "a" -> ["a"]
letterToNumber(C) ->
	io:format("C: ~w~n", C),
	if C == "a" -> 0;
	   C == "b" -> 1;
	   C == "c" -> 2;
	   C == "d" -> 3;
	   C == "e" -> 4;
	   C == "f" -> 5;
	   C == "g" -> 6;
	   C == "h" -> 7;
	   C == "i" -> 8;
	   C == "j" -> 9;
	   C == "k" -> 10;
	   C == "l" -> 11;
	   C == "m" -> 12;
	   C == "n" -> 13;
	   C == "o" -> 14;
	   C == "p" -> 15;
	   C == "q" -> 16;
	   C == "r" -> 17;
	   C == "s" -> 18;
	   C == "t" -> 19;
	   C == "u" -> 20;
	   C == "v" -> 21;
	   C == "w" -> 22;
	   C == "x" -> 23;
	   C == "u" -> 24;
	   C == "z" -> 25;
		true -> -1
	end.

base26to10(N) -> trunc(base26to10calc(reverse(N), 0)).

base26to10calc([], _Acc) -> 0;
base26to10calc([H | T], Acc) -> 
	io:format("H: ~w~n", H),
	io:format("ToNumber: ~w~n", letterToNumber([H])),
	Val = letterToNumber([H]) - letterToNumber("a"),
	Res = (pow(26, Acc) * Val) + base26to10calc(T, Acc+1),
	Res.
