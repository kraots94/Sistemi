%% @author Alessandro
%% @doc @todo Add description to city_map.


-module(city_map).
-include("records.hrl").
-include("globals.hrl").

-import('graph', [from_file/1, num_of_edges/1, num_of_vertices/1, del_graph/1, vertices/1, pprint/1, edges_with_weights/1]).
-import('dijkstra', [run/2]).
-import('my_util', [createPairs/2]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([init_city/0, load_nodes/0, print_nodes/1, delete_map/1, calculate_path/4]).

init_city() -> 
	Map = load_map(),
	City = #city{total_nodes = num_of_vertices(Map),
				 total_edges = num_of_edges(Map),
				 city_graph = Map,
				 nodes = load_nodes()},
	City.

print_nodes(Nodes) -> io:format("id name x y~n"),
					  print_node(Nodes).

delete_map(_Map) -> ok.

%% ====================================================================
%% User -> Reference to user
%% All three points are names in name format, not refs.
%% P1 -> Position Car after served user 
%% P2 -> Position user start
%% P3 -> Position user targat
%% City -> City refs
%% ====================================================================
calculate_path(User, P1, P2, P3) -> 
	io:format("Bella~n"),
	City_Graph = get_map(),
	ID_P1 = my_util:base26to10(P1),
	ID_P2 = my_util:base26to10(P2),
	_ID_P3 = my_util:base26to10(P3),
	io:format("id node: ~w~n", [ID_P1]),
	io:format("nodes: ~w~n", [num_of_edges(City_Graph)]),
	EdgesWeights = edges_with_weights(City_Graph),
	Out_Djsktra_P1 = dijkstra:run(City_Graph, ID_P1),
	PilaMacchina_POS_MACCHINA_TO_POS_USER = calculate_path_costs(EdgesWeights, Out_Djsktra_P1,ID_P1, ID_P2),
	io:format("========================~n"),
	io:format("~w~n", [PilaMacchina_POS_MACCHINA_TO_POS_USER]),
% [{COSTO, ID_NODO}, {COSTO, ID_NODO},{COSTO, ID_NODO},{COSTO, ID_NODO}]
% [{0,4}, {9, 6}, {6, 5}, {5, 2}]

% {[Costo P1-P2, P2-P3, P3-Colonnina], [tappe]}
%{[20,1,1],[{user, 0,"aabb", "partenza_taxi"}, {9, 6}, {6, 5}, {5, 2}]}
	io:format("========================~n"),
	io:format("~w~n", [Out_Djsktra_P1]),
	io:format("========================~n"),
	io:format("~w~n", [EdgesWeights]),
	del_graph(City_Graph),
	create_records(User, []).

%% ====================================================================
%% Internal functions
%% ====================================================================
get_map() ->
	load_map().
	
load_map() ->
	City_Map = graph:from_file(?FILE_MAP),
	City_Map.
% [{COSTO, ID_NODO}, {COSTO, ID_NODO},{COSTO, ID_NODO},{COSTO, ID_NODO}]
% {TOTAL_COST, [{0,4}, {9, 6}, {6, 5}, {5, 2}]}

calculate_path_costs(EdgesWeights, Djkstra_Out, _From_ID, Target_ID) -> 
	{_Cost, Points} = getDataPath(Djkstra_Out, Target_ID),
	Pairs = createPairs(Points, []),
	Costs = createCosts(Pairs, [], EdgesWeights),
	io:format("========================~n"),
	io:format("~w~n", [Costs]),
	[].

createCosts([], ACC, _EdgesWeights) -> ACC;
createCosts([{A,B} | T], ACC, EdgesWeights) ->
	NewACC = ACC ++ [{getWeight(A, B, EdgesWeights), B}],
	createCosts(T, NewACC, EdgesWeights).

%% From NODE with ID1 to ID2
getWeight(ID_1, ID_2, []) -> 
	if  ID_1 == ID_2 -> 0;
		true -> -1
	end;

getWeight(ID_1, ID_2, [H | T]) ->
	{{V1, V2}, W} = H,
	
	if  ID_1 == ID_2 -> 0;
	    (((V1 == ID_1) and (V2 == ID_2)) or ((V1 == ID_2) and (V2 == ID_1))) -> W;
		true -> getWeight(ID_1, ID_2, T)
	end.

getDataPath([], _Target_ID) -> {-1, {-1, []}};
getDataPath([H | T], Target_ID) -> 
	{ID, {Cost, Points}} = H,
	if ID == Target_ID -> {Cost, Points};
	   true -> getDataPath(T, Target_ID) 
	end.

load_nodes() -> 
	{ok, IO} = file:open(?FILE_NODES, [read]),
	{ok, [Total_Nodes]} = io:fread(IO, ">", "~d"),
	Nodes = read_node([], Total_Nodes, IO),
	Nodes.

read_node(Nodes, 0, _IO) -> Nodes;
read_node(Nodes, N, IO) -> 
	{ok, [Name, ID, X, Y]} = io:fread(IO, ">", "~a ~d ~d ~d"),
	New_node = #node
			{
				name = Name,
				id = ID,
				pos_x = X,
				pos_y = Y
			},
	NewNodes = Nodes++[New_node],
	
	read_node(NewNodes, N-1, IO).

print_node([]) -> ok;
print_node([H | Nodes]) -> 
	io:format("~w ~w ~w ~w~n",[H#node.id, H#node.name, H#node.pos_x, H#node.pos_y]),
	print_node(Nodes).

create_records(_User, _Points) -> {0, []}.