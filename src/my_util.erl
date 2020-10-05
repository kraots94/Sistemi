-module(my_util).
-import(lists,[reverse/1]). 
-import(math, [pow/2]).

-include("records.hrl").
-include("globals.hrl").

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
	RandomNumber = my_util:generate_random_number(2),
	Type = if RandomNumber == 1 -> car;
					true -> user
			end,
	DataInit = #dataInitGPSModule{
				pid_entity = N,
				type = Type,
				pid_server_gps = PID_GPS_Server,
				starting_pos = Pos,
				signal_power = ?GPS_MODULE_POWER,
				map_side = ?MAP_SIDE
			},
	PidNewGPS = gps_module:start_gps_module(DataInit),
	NewACC = [PidNewGPS] ++ ACC,
	createRandomEntity(PID_GPS_Server, Nodes, N-1, NewACC).

createRandomEntityFromPid(PID_GPS_Server, PID_ENT) ->
	Nodes = nodes_util:load_nodes(),
	Pos = nodes_util:getRandomPositionName(Nodes),
	RandomNumber = my_util:generate_random_number(2),
	Type = if RandomNumber == 1 -> car;
					true -> user
			end,
	DataInit = #dataInitGPSModule{
				pid_entity = PID_ENT,
				type = Type,
				pid_server_gps = PID_GPS_Server,
				starting_pos = Pos,
				signal_power = ?GPS_MODULE_POWER,
				map_side = ?MAP_SIDE
			},
	PidNewGPS = gps_module:start_gps_module(DataInit),
	PidNewGPS.

%	f(), make:all(), PID_GPS_Server = gps_server:start_gps_server(nodes_util:load_nodes()).
%	PID_ENTS = my_util:createRandomEntities(PID_GPS_Server, 20).
%	PID_GPS_CONSOLE = my_util:createRandomEntityFromPid(PID_GPS_Server, self()).
	
	