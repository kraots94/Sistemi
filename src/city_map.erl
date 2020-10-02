%% @author Alessandro
%% @doc @todo Add description to city_map.


-module(city_map).
-include("records.hrl").
-include("globals.hrl").

-import('graph', [from_file/1, num_of_edges/1, num_of_vertices/1, del_graph/1, vertices/1, pprint/1]).
-import('dijkstra', [run/2]).

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
%% ====================================================================
calculate_path(User, P1, P2, P3) -> 
	io:format("Bella~n"),
	City_Graph = get_map(),
	io:format("P1: ~w~n", P1),
	ID_P1 = my_util:base26to10(P1),
	_ID_P2 = my_util:base26to10(P2),
	_ID_P3 = my_util:base26to10(P3),
	io:format("id: ~w~n", ID_P1),
	Out_Djsktra = dijkstra:run(City_Graph, ID_P1),
	io:format("~p~n", Out_Djsktra),
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