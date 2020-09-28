-module(environment).
-export([start/0, generate_event/1, generate_random_number/0]).

start() ->
	io:format("Start Environment~n"),
	{T_M, T_S, T_m} = erlang:timestamp(),
	io:format("Current Timestamp: ~w ~w ~w ~n", [T_M, T_S, T_m]),
	rand:seed(exro928ss, {T_M, T_S, T_m}).

generate_event(N) ->
	if 	N < 0 ->  io:format("nothing happened~n");
		N < 10 -> io:format("spawn car~n");
	   	N < 15 -> io:format("nothing happened~n");
	   	N < 25 -> io:format("spawn client~n");
	   	N < 35 -> io:format("nothing happened~n");
	   	N < 45 -> io:format("client change target~n");
	   	N < 50 -> io:format("nothing happened~n");
	   	N < 60 -> io:format("car crash~n");
	   	N < 70 -> io:format("fix car~n");
		N < 75 -> io:format("nothing happened~n");
		N < 77 -> io:format("add node to map~n");
		N == 77 -> io:format("nothing happened~n");
		N < 80 -> io:format("add street to map~n");
		N == 80 -> io:format("nothing happened~n");
		N < 83 -> io:format("remove node to map~n");
		N == 83 -> io:format("nothing happened~n");
		N < 86 -> io:format("remove arrow to map~n");
		N < 95 -> io:format("nothing happened~n");
		N < 100 -> io:format("remove car~n");
		true -> io:format("nothing happened~n")
	end.

generate_random_number() ->
	N = rand:uniform(100),
	io:format("Generated number: ~w ~n", [N]),
	N.