%% @author Alessandro
%% @doc @todo Add description to nodes_util.


-module(nodes_util).

-include("records.hrl").
-include("globals.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([load_nodes/0, print_nodes/1, getNodeID/2, getNodeName/2]).

load_nodes() -> 
	{ok, IO} = file:open(?FILE_NODES, [read]),
	{ok, [Total_Nodes]} = io:fread(IO, ">", "~d"),
	Nodes = read_node([], Total_Nodes, IO),
	Nodes.

print_nodes(Nodes) -> io:format("id name x y~n"),
					  print_node(Nodes).

getNodeID(NodeName,Nodes) -> 
	getID(NodeName, Nodes).

getNodeName(NodeID,Nodes) -> 
	getName(NodeID,Nodes).
%% ====================================================================
%% Internal functions
%% ====================================================================

getID(_NodeName, []) -> -1;
getID(NodeName, [H | T]) -> 
	Name = H#node.name,
	ID = H#node.id,
	if NodeName =:= Name -> ID;
	   true -> getID(NodeName, T)
	end.

getName(_NodeID, []) -> "";
getName(NodeID, [H | T]) -> 
	Name = H#node.name,
	ID = H#node.id,
	if NodeID == ID -> Name;
	   true -> getName(NodeID, T)
	end.

read_node(Nodes, 0, _IO) -> Nodes;
read_node(Nodes, N, IO) -> 
	{ok, [Name, ID, X, Y]} = io:fread(IO, ">", "~s ~d ~d ~d"),
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
