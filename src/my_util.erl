-module(my_util).
-export([println/1, add/2, prod/2]).

println(What) -> 
	io:format("Messaggio: ~p~n", [What]).

add(A,B) ->
	A + B.

prod(A,B) ->
	A * B.