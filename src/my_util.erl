-module(my_util).
-export([println/1, add/2]).

println(What) -> 
	io:format("Messaggio: ~p~n", [What]).

add(A,B) ->
	A + B.