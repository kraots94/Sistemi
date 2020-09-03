-module(my_util).
-export([println/1, add/2, prod/2, temperature_converter/2, temperature_converter/3]).

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

