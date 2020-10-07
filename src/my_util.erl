-module(my_util).
-import(lists,[reverse/1]). 
-import(math, [pow/2]).

-include("records.hrl").
-include("globals.hrl").

-export([print_debug_message/3, 
		 calculateSquaredDistance/2, 
		 createRandomEntities/2, 
		 createPairs/2, 
		 createPairsFromList/1,
		 generate_random_number/1]).

%print normal text
println(String) ->
	io:format(String ++ "~n", []).

%print text e variabile
println(String,Var) ->
	io:format(String ++ " ~p~n" , [Var]).

%print list
printList(String,List) -> 
	io:format(String ++ " ~w~n" , [List]).

init_random_generator() ->
	println("Initiating random generatior.~n"),
	{T_M, T_S, T_m} = erlang:timestamp(),
	io:format("Current Timestamp: ~w ~w ~w ~n", [T_M, T_S, T_m]),
	rand:seed(exro928ss, {T_M, T_S, T_m}).

% Generates a number 1 <= x <= MAX
generate_random_number(MAX) ->
	N = rand:uniform(MAX),
%	io:format("Generated number: ~w ~n", [N]),
	N.

createPairsFromList(List) -> createPairs(List, []).

createPairs([], ACC) -> ACC;

createPairs([A , B | []], ACC) -> 
	NEW_LIST = ACC ++ [{A, B}],
	NEW_LIST;

createPairs([A | []], _ACC) ->
	[{A,A}];

createPairs([A , B | Tail], ACC) ->
	NEW_LIST = ACC ++ [{A, B}],
	createPairs([B] ++ Tail, NEW_LIST).

arraySum(_A) -> 0.

calculateSquaredDistance({Px, Py}, {Qx, Qy}) ->
	Diff_1 = Qx - Px,
	Diff_2 = Qy - Py,
	SquaredDistance = Diff_1 * Diff_1 + Diff_2 * Diff_2,
	SquaredDistance.

createRandomEntities(PID_GPS_Server, N) ->
	Nodes = nodes_util:load_nodes(),
	createRandomEntity(PID_GPS_Server, Nodes, N, []).

createRandomEntity(_PID_GPS_Server,_Nodes, 0, ACC) -> ACC;
createRandomEntity(PID_GPS_Server, Nodes, N, ACC) -> 
	Pos = nodes_util:getRandomPositionName(Nodes),
	PID_CAR = macchina_ascoltatore:start({Pos, PID_GPS_Server, 0}),
	NewACC = [PID_CAR] ++ ACC,
	createRandomEntity(PID_GPS_Server, Nodes, N-1, NewACC).

print_debug_message(PID, Format, Data) ->
	Message = construct_string(Format, Data),
	io:format("{~w} - ~p ~n", [PID, Message]).

construct_string(Format, Data) ->
	lists:flatten(io_lib:format(Format, Data)).  