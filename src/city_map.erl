%% @author Alessandro
%% @doc @todo Add description to city_map.


-module(city_map).
-include("records.hrl").
-include("globals.hrl").

-import('graph', [from_file/1, num_of_edges/1, num_of_vertices/1, del_graph/1, pprint/1, edges_with_weights/1]).
-import('graph_utils', [getDataPath/2, getWeight/3]).
-import('dijkstra', [run/2]).
-import('my_util', [createPairs/2]).
-import('nodes_util', [load_nodes/0, getNodeID/2, getNodeName/2]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([init_city/0, delete_map/1, calculate_path/4]).

init_city() -> 
	Map = load_map(),
	City = #city{total_nodes = num_of_vertices(Map),
				 total_edges = num_of_edges(Map),
				 city_graph = Map,
				 nodes = load_nodes()},
	City.


delete_map(_Map) -> ok.

%% ====================================================================
%% User -> Reference to user
%% All three points are names in name format, not refs.
%% P1 -> Position Car after served last user in queue
%% P2 -> Position new user start
%% P3 -> Position new user target
%% P4 -> Position nearest column
%% City -> City refs
%% ====================================================================
calculate_path(User, P1, P2, P3) -> 
	City_Graph = get_map(),
	Nodes = load_nodes(),
	ID_P1 = getNodeID(P1, Nodes),
	ID_P2 = getNodeID(P2, Nodes),
	ID_P3 = getNodeID(P3, Nodes),
%	ID_P4 = getClosestRecharchingCol(ID_P3),
	Out_Djsktra_P1 = dijkstra:run(City_Graph, ID_P1),
	Out_Djsktra_P2 = dijkstra:run(City_Graph, ID_P2),
%	Out_Djsktra_P3 = dijkstra:run(City_Graph, ID_P3),
	EdgesWeights = edges_with_weights(City_Graph),
	Queue_Car_User = calculate_path_costs(EdgesWeights, Out_Djsktra_P1,ID_P1, ID_P2),
	Queue_User_Target = calculate_path_costs(EdgesWeights, Out_Djsktra_P2,ID_P2, ID_P3),
%	Queue_Target_Column = calculate_path_costs(EdgesWeights, Out_Djsktra_P2,ID_P3, ID_P4),
	Queue_Target_Column = {0, []},
	Out_Records = create_records(User, Nodes, Queue_Car_User, Queue_User_Target, Queue_Target_Column),
	del_graph(City_Graph),
	Out_Records.

%% ====================================================================
%% Internal functions
%% ====================================================================
get_map() ->
	load_map().
	
load_map() ->
	City_Map = graph:from_file(?FILE_MAP),
	City_Map.

% Ritorna questo
% [{COSTO, ID_NODO}, {COSTO, ID_NODO},{COSTO, ID_NODO},{COSTO, ID_NODO}]
% {TOTAL_COST, [{0,4}, {9, 6}, {6, 5}, {5, 2}]}
calculate_path_costs(EdgesWeights, Djkstra_Out, _From_ID, Target_ID) -> 
	{Cost, Points} = getDataPath(Djkstra_Out, Target_ID),
	Pairs = createPairs(Points, []),
	CarQueue = createCosts(Pairs, [], EdgesWeights),
	OutCosts = {Cost, CarQueue},
	OutCosts.

createCosts([], ACC, _EdgesWeights) -> ACC;
createCosts([{A,B} | T], ACC, EdgesWeights) ->
	NewACC = ACC ++ [{getWeight(A, B, EdgesWeights), B}],
	createCosts(T, NewACC, EdgesWeights).

% {TOTAL_COST, [{0,4}, {9, 6}, {6, 5}, {5, 2}]}
% {TOTAL_COST, [{0,4}, {9, 6}, {6, 5}, {5, 2}]}
% {TOTAL_COST, [{0,4}, {9, 6}, {6, 5}, {5, 2}]}
% user_start, user_target, intermediate, column_path, column_end
% {[Costo P1-P2, P2-P3, P3-Colonnina], [tappe]}
%{[20,1,1],[{user, 0,"aabb", "partenza_taxi"}, {9, 6}, {6, 5}, {5, 2}]}
create_records(User, Nodes, Queue_Car_User, Queue_User_Target, Queue_Target_Column) ->
	{Cost_1, Queue_1} = Queue_Car_User,
	{Cost_2, Queue_2} = Queue_User_Target,
	{Cost_3, _Queue_3} = Queue_Target_Column,
	Total_Costs = [Cost_1, Cost_2, Cost_3],
	{Total_Costs, createRecordToUser(Queue_1, User, Nodes, []) ++ createRecordToTarget(Queue_2, User, Nodes, [])}.

% user_start, user_target, intermediate, column_path, column_end, none
%-record(tappa, {user, type, t, node_name}).

createRecordToUser([], _Username, _Nodes, ACC) -> ACC;
createRecordToUser([H | T], Username, Nodes, ACC) ->
   {Cost, NextHop} = H,
	NextHopName = getNodeName(NextHop, Nodes),
	Temp = #tappa
			{
				user = Username,
				type = none,
				t = Cost,
				node_name = NextHopName
			},
	NewTappa =  if T =:= [] ->  Temp#tappa{type = user_start};
					true -> 	Temp#tappa{type = intermediate}
			 	end,

   NewACC = ACC ++ [NewTappa],
	createRecordToUser(T, Username, Nodes, NewACC).


createRecordToTarget([], _Username, _Nodes, ACC) -> ACC;
createRecordToTarget([H | T], Username, Nodes, ACC) ->
   {Cost, NextHop} = H,
   	NextHopName = getNodeName(NextHop, Nodes),
	Temp = #tappa
			{
				user = Username,
				type = none,
				t = Cost,
				node_name = NextHopName
			},
	NewTappa =  if T =:= [] ->  Temp#tappa{type = user_target};
					true -> 	Temp#tappa{type = intermediate}
			 	end,

   NewACC = ACC ++ [NewTappa],
	createRecordToTarget(T, Username, Nodes, NewACC).
