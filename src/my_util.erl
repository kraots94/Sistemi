-module(my_util).
-export([println/1, 
		 add/2, prod/2, 
		 temperature_converter/2, temperature_converter/3,
		 test_patternMatchingConPluriMatch/1,test_caseRitornaValore/1, 
		 len/1, acc_len/1, acc_fact/1, duplicate/2, reverse/1, acc_reverse/1, sublist/2, acc_sublist/2,
		 zip/2, acc_zip/2,
		 boosted_quicksort/1]).

println(What) -> 
	io:format("Messaggio: ~p~n", [What]).

add(A,B) ->
	A + B.

prod(A,B) ->
	A * B.

temperature_converter({Current_scale, T}, Target_scale) ->
	{Target_scale, temperature_converter(Current_scale, T, Target_scale)}.

temperature_converter(kelvin, T, kelvin) -> T;
temperature_converter(kelvin, T, celsius) ->  T - 273.15;
temperature_converter(kelvin, T, fahrenheit) -> (T - 273.15) * 9 / 5 + 32;

temperature_converter(celsius, T, kelvin) -> T + 273.15;
temperature_converter(celsius, T, celsius) ->  T;
temperature_converter(celsius, T, fahrenheit) -> T * 9 / 5 + 32;

temperature_converter(fahrenheit, T, kelvin) -> (T + 459.67) * 5 / 9;
temperature_converter(fahrenheit, T, celsius) ->  T - (T - 32) * 5 / 9;
temperature_converter(fahrenheit, T, fahrenheit) -> T.

%test di matching dove ci sono 2 match, infatti con input {preciso,T}
%il match avviene con entrambe le funzioni ma viene selezionata la prima che matcha in ordine di scrittura 
test_patternMatchingConPluriMatch({preciso, T}) -> println(T);
%underscore inserito per non avere warning di mancato utilizzo var input
test_patternMatchingConPluriMatch({_,_}) -> println("non preciso").

%dimostrazione che case (ma anche if) è una espressione e in quanto tale torna un valore
%come in scheme e tutti i linguaggi funz.
test_caseRitornaValore(ToTest) -> 
	ValueRet = case ToTest of 
		"caso1" -> 1;
		"caso2" -> 2;
		_ -> 3
	end,
	ValueRet + 1000.


% Calcolo del numero degli elementi di una lista con ricorsione
len([]) -> 0;
len([_ | L]) -> 1 + len(L).

% Calcolo del numero degli elementi di una lista con ricorsione e accumulatore
acc_len(L) -> acc_len(L, 1).

acc_len([], Acc) -> Acc;
acc_len([_ | T], Acc) -> acc_len(T, Acc +1). 

% Fattoriale con accumulatore
acc_fact(N) -> acc_fact(N, 1).

acc_fact(0, Acc) -> Acc;
acc_fact(N, Acc) when N > 0 -> acc_fact(N-1, N * Acc).

% Crea una lista di N elementi con Term come elemento
% duplicate(0, _) -> [];
% duplicate(N, Term) when N > 0 -> [Term | duplicate(N - 1, Term)].

% Verione con accumulutatore
duplicate(N, Term) -> duplicate(N, Term, []).
duplicate(0, _ , List) -> List;
duplicate(N, Term, List) -> duplicate(N - 1, Term, [Term | List]).

reverse([]) -> [];
reverse([H | T]) -> reverse(T) ++ [H].

acc_reverse(L) -> acc_reverse(L, []).
acc_reverse([], Reversed) -> Reversed;
acc_reverse([H | T], Reversed) -> acc_reverse(T, [H | Reversed]).

sublist(_, 0) -> [];
sublist([], _) -> [];
% Versione alternativa
sublist([H | T], N) when N > 0 -> [H] ++ sublist(T, N-1).
% sublist([H | T], N) when N > 0 -> [H | sublist(T, N-1)].

acc_sublist(L, N) -> acc_reverse(acc_sublist(L, N, [])).

acc_sublist(_, 0, OutList) -> OutList;
acc_sublist([], _, OutList) -> OutList;
acc_sublist([H | T], N, OutList) when N > 0 -> acc_sublist(T, N -1, [H | OutList]).

zip([], _) -> [];
zip(_, []) -> [];
zip([H1 | T1], [H2 | T2]) -> [{H1, H2}] ++ zip(T1, T2).

acc_zip(L1, L2) -> acc_reverse(acc_zip(L1,L2,[])).

acc_zip([], _, Acc_List) -> Acc_List;
acc_zip(_, [], Acc_List) -> Acc_List;
acc_zip([H1 | T1], [H2 | T2], Acc_List) -> acc_zip(T1,T2, [{H1, H2} | Acc_List]).

boosted_quicksort([]) -> [];
boosted_quicksort([Pivot | Rest]) -> 
	{Smaller, Equal, Larger} = partition(Pivot, Rest, [], [], []),
	boosted_quicksort(Smaller) ++ [Pivot | Equal] ++ boosted_quicksort(Larger).

partition(_, [], Smaller, Equal, Larger) -> {Smaller, Equal, Larger};
partition(Pivot, [H | T], Smaller, Equal, Larger) ->
	if H > Pivot  -> partition(Pivot, T, Smaller, Equal, [H | Larger]);
	   H < Pivot  -> partition(Pivot, T, [H | Smaller], Equal, Larger);
	   H == Pivot -> partition(Pivot, T, Smaller, [H | Equal], Larger)
	end.
