%% @author Alessandro
%% @doc @todo Add description to utilities.


-module(utilities).
-include("globals.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([println/1, 
		 println/2, 
		 printList/2,
		 print_debug_message_raw/1,
		 print_debug_message/1,
		 print_debug_message/2,
		 print_debug_message/3, 
		 calculateSquaredDistance/2, 
		 generate_random_number/1,
		 createRandomEntities/2, 
		 createPairsFromList/1]).

%print normal text
%print normal text
println(String) ->
	io:format(String ++ "~n", []).

%print text e variabile
println(String,Var) ->
	io:format(String ++ " ~p~n" , [Var]).

%print list
printList(String,List) -> 
	io:format(String ++ " ~w~n" , [List]).

print_debug_message_raw(Text) -> io:format(Text).
print_debug_message(Message) -> print_debug_message("", Message, none).
print_debug_message(Format, Data) -> print_debug_message("", Format, Data).
print_debug_message(PID, Format, Data) ->
	if PID == "" -> 
		   if Data == none -> 
				  		io:format("[Debug] ~p ~n", [Format]);
					true ->	
						io:format("[Debug] "++Format++"~n", Data)
		   end;
		true -> 
			if Data == none -> 
					io:format("[Debug] {~w} - ~p ~n", [PID, Format]);
				true ->	
					io:format("[Debug] {~w} - "++Format++"~n", [PID, Data])
			end	   
	end.
	

calculateSquaredDistance({Px, Py}, {Qx, Qy}) ->
	Diff_1 = Qx - Px,
	Diff_2 = Qy - Py,
	SquaredDistance = Diff_1 * Diff_1 + Diff_2 * Diff_2,
	SquaredDistance.

createRandomEntities(PID_GPS_Server, N) ->
	Nodes = nodes_util:load_nodes(),
	createRandomEntity(PID_GPS_Server, Nodes, N, []).

createPairsFromList(List) -> createPairs(List, []).

% Generates a number 1 <= x <= MAX
generate_random_number(MAX) ->
	N = rand:uniform(MAX),
% 	print_debug_message("", "Generated number: ~w ~n", N)
	N.

%% ====================================================================
%% Internal functions
%% ====================================================================

createRandomEntity(_PID_GPS_Server,_Nodes, 0, ACC) -> ACC;
createRandomEntity(PID_GPS_Server, Nodes, N, ACC) -> 
	Pos = nodes_util:getRandomPositionName(Nodes),
	PID_CAR = macchina_ascoltatore:start({Pos, PID_GPS_Server, 0}),
	NewACC = [PID_CAR] ++ ACC,
	createRandomEntity(PID_GPS_Server, Nodes, N-1, NewACC).

createPairs([], ACC) -> ACC;

createPairs([A , B | []], ACC) -> 
	NEW_LIST = ACC ++ [{A, B}],
	NEW_LIST;

createPairs([A | []], _ACC) ->
	[{A,A}];

createPairs([A , B | Tail], ACC) ->
	NEW_LIST = ACC ++ [{A, B}],
	createPairs([B] ++ Tail, NEW_LIST).
