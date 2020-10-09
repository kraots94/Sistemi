%% @author Alessandro
%% @doc @todo Add description to city_map.

-module(city_map).
-include("records.hrl").
-include("globals.hrl").

-record(column_entity_data,{dist, node_name}).

-import('graph', [from_file/1, num_of_edges/1, num_of_vertices/1, del_graph/1, pprint/1, edges_with_weights/1]).
-import('graph_utils', [getDataPath/2, getWeight/3]).
-import('dijkstra', [run/2]).
-import('nodes_util', [load_nodes/0, load_charging_cols/0, getNodeID/2, getNodeName/2, getPositionFromNodeName/2]).
-import('utilities', [print_debug_message/1, print_debug_message/2, print_debug_message/3, createPairsFromList/1, calculateSquaredDistance/2]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([init_city/0, calculate_path/2, get_nearest_col/3, create_records/5]).

init_city() -> 
	Map = load_map(),
	City = #city{total_nodes = num_of_vertices(Map),
				 total_edges = num_of_edges(Map),
				 city_graph = Map,
				 nodes = load_nodes(),
				 column_positions = load_charging_cols()},
	City.

calculate_path(CityData, {P, Q}) -> 
	{City_Graph, Nodes} = CityData,
	ID_P = getNodeID(P, Nodes),
	ID_Q = getNodeID(Q, Nodes),
	Out_Djsktra_P = dijkstra:run(City_Graph, ID_P),
	EdgesWeights = edges_with_weights(City_Graph),
	Queue_Car_P_Q = calculate_path_costs(EdgesWeights, Out_Djsktra_P,ID_P, ID_Q),
	Queue_Car_P_Q.

get_nearest_col(CurrentPosName, Nodes, Cols_Nodes) -> 
	getNearestColumnNodeName(CurrentPosName, Nodes, Cols_Nodes).

%% ====================================================================
%% Internal functions
%% ====================================================================
load_map() ->
	City_Map = graph:from_file(?FILE_MAP),
	City_Map.

% Ritorna questo
% [{COSTO, ID_NODO}, {COSTO, ID_NODO},{COSTO, ID_NODO},{COSTO, ID_NODO}]
% {TOTAL_COST, [{0,4}, {9, 6}, {6, 5}, {5, 2}]}
calculate_path_costs(EdgesWeights, Djkstra_Out, _From_ID, Target_ID) -> 
	{Cost, Points} = getDataPath(Djkstra_Out, Target_ID),
	Pairs = createPairsFromList(Points),
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
% user_start, user_target, intermediate, column (nodo posizione colonnina)
% {{Cost_To_Target, [Tappe]}, {Cost_To_Column, [Tappe]}}
create_records(User, Nodes, Queue_Car_User, Queue_User_Target, Queue_Target_Column) ->
	{Cost_1, Queue_1} = Queue_Car_User,
	{Cost_2, Queue_2} = Queue_User_Target,
	{Cost_3, Queue_3} = Queue_Target_Column,
	Queue_To_User_Dest = createRecordToUser(Queue_1, User, Nodes, []) ++ createRecordToTarget(Queue_2, User, Nodes, []),
	Queue_To_Column =  createRecordToColumn(Queue_3, Nodes, []),
	{{Cost_1, Cost_2, Cost_3}, Queue_To_User_Dest, Queue_To_Column}.

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

createRecordToColumn([], _Nodes, ACC) -> ACC;
createRecordToColumn([H | T], Nodes, ACC) ->
   {Cost, NextHop} = H,
   	NextHopName = getNodeName(NextHop, Nodes),
	Temp = #tappa
			{
				user = none,
				type = none,
				t = Cost,
				node_name = NextHopName
			},
	NewTappa =  if T =:= [] ->  Temp#tappa{type = column};
					true -> 	Temp#tappa{type = intermediate}
			 	end,

   NewACC = ACC ++ [NewTappa],
	createRecordToColumn(T, Nodes, NewACC).

calculateColsDistances(CurrentPosName, Nodes, Cols_Nodes) ->
	{StartNode_X, StartNode_Y} = getPositionFromNodeName(CurrentPosName, Nodes),
	
	MapFunc = fun(Node) ->	  
				Curr_X = Node#node.pos_x,
				Curr_Y = Node#node.pos_y,
				SquaredDistanceNodes = calculateSquaredDistance({StartNode_X, StartNode_Y}, {Curr_X, Curr_Y}),
				#column_entity_data{
					dist = SquaredDistanceNodes, 
					node_name = Node#node.name
					}
			end,

	ColumnNodesDistances = lists:map(MapFunc, Cols_Nodes),	
	ColumnNodesDistances.

getNearestColumnNodeName(CurrentPosName, Nodes, Cols_Nodes) ->
	NodesDistances = calculateColsDistances(CurrentPosName, Nodes, Cols_Nodes),
	SortFun = fun(A, B) ->  A#column_entity_data.dist < B#column_entity_data.dist end,
	Sorted_Nodes = lists:sort(SortFun, NodesDistances),
	NearestColumn = hd(Sorted_Nodes),
	NearestColumn#column_entity_data.node_name.
