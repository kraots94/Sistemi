%% @author Alessandro
%% @doc @todo Add description to city_map.


-module(city_map).
-include("records.hrl").
-include("globals.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([load_map/0, load_nodes/0, print_nodes/1]).

load_map() -> graph:from_file(?FILE_MAP).

load_nodes() -> 
	{ok, IO} = file:open(?FILE_NODES, [read]),
	{ok, [Total_Nodes]} = io:fread(IO, ">", "~d"),
	Nodes = read_node([], Total_Nodes, IO),
	Nodes.


print_nodes(Nodes) -> io:format("id name x y~n"),
					  print_node(Nodes).

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
