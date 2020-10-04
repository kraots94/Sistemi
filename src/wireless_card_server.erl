-module(wireless_card_server).

-record(nodeDistance,{dist, entities}).
-record(entityPosition, {pid, position}). 
-record(nodeEntities, {nodeData, entities, nearNodes}).
-record(wirelessCardServerState, {
				entityPositions, %lista di entityPosition ossia di roba {Pid, {x,y}}
				nodes,
				nodesMap}).

-include("records.hrl").
-include("globals.hrl").

-import('my_util',[println/1, println/2]).
-import('send', [send_message/2, send_message/3]).
-export([start_wireless_server/1, init/1, end_wireless_server/1, sendPosToGps/2, deleteMyLocationTracks/1, getNearestCar/1, printInternalState/1]).

%f(), make:all(), PID_Wireless_Server = wireless_card_server:start_wireless_server(nodes_util:load_nodes()).
%PID_Wireless_Server ! {self(), {setPosition, "a"}},
%PID_Wireless_Server ! {1, {setPosition, "c"}},
%PID_Wireless_Server ! {2, {setPosition, "c"}},
%PID_Wireless_Server ! {3, {setPosition, "a"}},
%PID_Wireless_Server ! {4, {setPosition, "e"}},
%PID_Wireless_Server ! {5, {setPosition, "f"}},
%PID_Wireless_Server ! {6, {setPosition, "b"}},
%PID_Wireless_Server ! {7, {setPosition, "a"}}.

%% ====================================================================
%% API functions
%% ====================================================================

sendPosToGps(WirelessCardPid, Position) ->
	WirelessCardPid ! {self(), {setPosition, Position}}.
	%WirelessCardPid ! {foo, {printState}}.

deleteMyLocationTracks(WirelessCardPid) ->
	WirelessCardPid ! {self(), {removeEntity}}.

%torna entita' piu' vicina per ora (non necessariamente macchina)
getNearestCar(WirelessCardPid) ->
	WirelessCardPid ! {self(), {getNearestCar, self()}}.

%for debugging porp.
printInternalState(WirelessCardPid) ->
	WirelessCardPid ! {printState}.

%% ====================================================================
%% Internal Functions
%% ====================================================================

start_wireless_server(Nodes) -> spawn_link('wireless_card_server', init, [Nodes]).

init(Nodes) -> 
	S = #wirelessCardServerState{entityPositions = [], nodes = initNodes(Nodes), nodesMap = Nodes},
	loop(S).

initNodes(Nodes) -> initNode(Nodes, []).

initNode([], ACC) -> ACC;
initNode([H | T], ACC) -> 
	StateNode = #nodeEntities{nodeData = H,
							entities = []},
	NewACC = [StateNode] ++ ACC,
	initNode(T, NewACC).

%metti invio msg al pid
loop(S) ->
	receive 
		{Pid, {register, _Type, Pos}} 				->  io:format("Have to ADD: ~w~n", [Pid]),
								  		   				NewState = updateEntityPosition(S, Pid, Pos),
								           				loop(NewState);
		{Pid, {setPosition, NewPos}}   	    		->  NewState = updateEntityPosition(S, Pid, NewPos),
									       			    loop(NewState);
		{Pid, {getPosition}}            			->  {Pos, _Res} = getPos(S#wirelessCardServerState.entityPositions, Pid),
										   			    Pid ! Pos,
									   					loop(S);
		{printState}			 	    			->  my_util:println("Wireless Server State", S),
									       				loop(S);
		{Pid, {removeEntity}}   	    			->  NewState = removeEntity(S, Pid), 
									       				loop(NewState);
		{Pid, {getNearEntities, CurrentPos, Power}} -> 
														Pid ! getNearEntities(S, CurrentPos, Power, Pid),
														loop(S);
		{Pid, {getNearestCar, CurrentPos}}			-> 
														Pid ! getNearestCar(S, CurrentPos, Pid),
														loop(S);
		{Pid, Ref, terminate} ->
			send_message(Pid, {Ref, ok}),
			println("Exiting wireless server loop~n");
        Unknown ->
            %% do some logging here too
            io:format("Unknown message: ~p~n", [Unknown]),
            loop(S)
	end.

getNearestCar(State, CurrentPosName, Pid) ->
	Nodes = State#wirelessCardServerState.nodesMap,
	{StartNode_X, StartNode_Y} = nodes_util:getPositionFromNodeName(CurrentPosName, Nodes),
	NodesWireless = State#wirelessCardServerState.nodes,
	
	MapFunc = fun(Node) -> %CurrentNodeName = X#nodeEntities.nodeData#node.name,
							  NodeData = Node#nodeEntities.nodeData,
							  Curr_X = NodeData#node.pos_x,
							  Curr_Y = NodeData#node.pos_y,
							  Diff_1 = Curr_X - StartNode_X,
							  Diff_2 = Curr_Y - StartNode_Y,
							  DistNodes = Diff_1 * Diff_1 + Diff_2 * Diff_2,
							  #nodeDistance{dist = DistNodes,
											entities = Node#nodeEntities.entities
											}
						 end,
	EntitiesDistances = lists:map(MapFunc, NodesWireless),	
	SortFun = fun(A, B) -> 
					  A#nodeDistance.dist < B#nodeDistance.dist
					  end,	
	Sorted_Nodes = lists:sort(SortFun, EntitiesDistances),
	Sorted_Pids = clearData(Sorted_Nodes, []),
	RemovedSelf = lists:delete(Pid, Sorted_Pids),
	hd(RemovedSelf).

clearData([], ACC) -> ACC;
clearData([H | T], ACC) -> 
	Entities = H#nodeDistance.entities,
	NewACC = ACC ++ Entities,
	clearData(T, NewACC).

getNearEntities(State, CurrentPosName, Max_Distance, PID) ->
	Nodes = State#wirelessCardServerState.nodesMap,
	{StartNode_X, StartNode_Y} = nodes_util:getPositionFromNodeName(CurrentPosName, Nodes),
	SquaredDistance = Max_Distance * Max_Distance,
	
	NodesWireless = State#wirelessCardServerState.nodes,
	FilterFunc = fun(Node) -> %CurrentNodeName = X#nodeEntities.nodeData#node.name,
							  NodeData = Node#nodeEntities.nodeData,
							  Curr_X = NodeData#node.pos_x,
							  Curr_Y = NodeData#node.pos_y,
							  Diff_1 = Curr_X - StartNode_X,
							  Diff_2 = Curr_Y - StartNode_Y,
							  Res = Diff_1 * Diff_1 + Diff_2 * Diff_2,
							  Res < SquaredDistance
						 end,
	NearNodes = lists:filter(FilterFunc, NodesWireless),

	NodesEntities = getEntities(NearNodes, []),
	OutEntities = lists:delete(PID, NodesEntities),
	OutEntities.
	
getEntities([], ACC) -> ACC;	
getEntities([H | T ], ACC) -> 
	NewACC = ACC ++ H#nodeEntities.entities,
	getEntities(T, NewACC).
	
removeEntity(State, Pid) ->
	CurrentEntitiesPositions = State#wirelessCardServerState.entityPositions,
	{Pos, EntityIsInHash} = getPos(CurrentEntitiesPositions, Pid),
	NewState = if EntityIsInHash == ok -> S1 = removeEntityFromPosition(State, Pos, Pid),
										  Filter = fun(N) -> 
														   N_Pid = N#entityPosition.pid,
														   not (Pid == N_Pid)
														   end,
										  NewHashmap = lists:filter(Filter, CurrentEntitiesPositions),
										  S1#wirelessCardServerState{entityPositions = NewHashmap};
									true -> State
								end,
	NewState.

updateEntityPosition(State, Pid, NewNodeName) ->
	CurrentEntitiesPositions = State#wirelessCardServerState.entityPositions,

	{Pos, EntityIsInHash} = getPos(CurrentEntitiesPositions, Pid),

	% rimuovere il pid da dov'è se l'entità è già presente
	StateRemovedEntityFromNode = if EntityIsInHash == ok -> removeEntityFromPosition(State, Pos, Pid);
									true -> State
								end,

	% metterlo nella lista nel nuovo nodo
	StateAddedEntityInNode = insertEntityInPosition(StateRemovedEntityFromNode, NewNodeName, Pid),

	% aggiornare la sua posizione nella hash
	FinalState = if EntityIsInHash == ok -> 
						F = fun(Entity) ->
								E_Pid = Entity#entityPosition.pid,
								Position = Entity#entityPosition.position,
								NewPosition = if E_Pid =:= Pid -> NewNodeName;
													true -> Position
												end,
								Entity#entityPosition{position = NewPosition}
							end,
						NewEntityPositions = lists:map(F, CurrentEntitiesPositions),
						NewState = StateAddedEntityInNode#wirelessCardServerState{
							entityPositions = NewEntityPositions	
						},
						NewState;
					true ->
						NewRecord = #entityPosition{pid = Pid, position = NewNodeName},
						NewEntityPositions = CurrentEntitiesPositions ++ [NewRecord],
						NewState = StateAddedEntityInNode#wirelessCardServerState{
								entityPositions = NewEntityPositions			
							},
						NewState
				end,
	FinalState.

insertEntityInPosition(State, NodeName, Pid) ->
	CurrentNodesData = State#wirelessCardServerState.nodes,
	F = fun(Node) ->
			Name = Node#nodeEntities.nodeData#node.name,
			EntitiesInNode = Node#nodeEntities.entities,
			NewEntities = if Name =:= NodeName -> EntitiesInNode ++ [Pid];
							true -> EntitiesInNode
						end,
			Node#nodeEntities{entities = NewEntities}
		end,
	NewNodesData = lists:map(F, CurrentNodesData),
	State#wirelessCardServerState{nodes = NewNodesData}.

removeEntityFromPosition(State, NodeName, Pid) -> 
	CurrentNodesData = State#wirelessCardServerState.nodes,
	F = fun(Node) ->
			Name = Node#nodeEntities.nodeData#node.name,
			EntitiesInNode = Node#nodeEntities.entities,
			NewEntities = if Name =:= NodeName -> lists:filter(fun(X) -> (not (X == Pid)) end, EntitiesInNode);
							true -> EntitiesInNode
						end,
			Node#nodeEntities{entities = NewEntities}
		end,
	NewNodesData = lists:map(F, CurrentNodesData),
	State#wirelessCardServerState{nodes = NewNodesData}.

getListEntitiesInPosition(NodesEntities, NodeName) ->
	{_Name, Entities} = hd(lists:filter(fun(X) -> (X#nodeEntities.nodeData#node.name == NodeName) end, NodesEntities)),
	Entities.

getPos(Posizioni, ToSearch) ->
	Out = lists:filter(fun(X) -> (X#entityPosition.pid == ToSearch) end, Posizioni),
	if Out =:= [] -> {[], none};
		true -> 
			El = hd(Out),
			{El#entityPosition.position, ok}
	end.


end_wireless_server(Pid) ->
	println("Killing Wireless Server"),
	Ref = erlang:monitor(process, Pid),
	send_message(Pid, {self(), Ref, terminate}),
	
	receive
		{Ref, ok} ->
			erlang:demonitor(Ref, [flush]),
			ok;
		{'DOWN', Ref, process, Pid, Reason} ->
			erlang:error(Reason)
	after 5000 ->
		erlang:error(timeout)
	end.
