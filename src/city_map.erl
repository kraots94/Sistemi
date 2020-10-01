%% @author Alessandro
%% @doc @todo Add description to city_map.


-module(city_map).
-include("records.hrl").
-include("globals.hrl").

-import('graph', [from_file/1, num_of_edges/1, num_of_vertices/1, del_graph/1, vertices/1, pprint/1]).
-import('dijkstra', [run/2]).
%%	
_Verticies = vertices(City_Map),
	pprint(City_Map),
	Out_Djsktra = dijkstra:run(City_Map, 6),
	io:format("========================~n"),
	io:format("~w~n", [Out_Djsktra]),
	Nodes = load_nodes(),
	print_nodes(Nodes),
	io:format("Total nodes: ~w~n", [num_of_vertices(City_Map)]),
	io:format("Total edges: ~w~n", [num_of_edges(City_Map)]),
	del_graph(City_Map),
%% ====================================================================
%% API functions
%% ====================================================================
-export([load_map/0, load_nodes/0, print_nodes/1, delete_map/1, calculate_path/4]).

load_map() -> graph:from_file(?FILE_MAP).

load_nodes() -> 
	{ok, IO} = file:open(?FILE_NODES, [read]),
	{ok, [Total_Nodes]} = io:fread(IO, ">", "~d"),
	Nodes = read_node([], Total_Nodes, IO),
	Nodes.


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
calculate_path(User, _P1, _P2, _P3) -> create_records(User, []).

%% ====================================================================
%% Internal functions
%% ====================================================================

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