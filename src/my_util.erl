-module(my_util).
-export([println/1, add/2, prod/2, temperature_converter/2, temperature_converter/3
		,test_patternMatchingConPluriMatch/1,test_caseRitornaValore/1]).

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
test_patternMatchingConPluriMatch(_Tupla) -> println("non preciso").

%dimostrazione che case (ma anche if) è una espressione e in quanto tale torna un valore
%come in scheme e tutti i linguaggi funz.
test_caseRitornaValore(ToTest) -> 
	ValueRet = case ToTest of 
		"caso1" -> 1;
		"caso2" -> 2;
		_ -> 3
	end,
	ValueRet + 1000.
