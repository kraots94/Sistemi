-module(wireless_card_server).

-record(nodeDistance,{dist, entities}).
-record(entity, {pid, type, position}). 
-record(nodeEntities, {nodeData, entities}).
-record(wirelessCardServerState, {
				entities,
				nodes,
				nodesMap}).

-include("records.hrl").
-include("globals.hrl").

-import('my_util',[println/1, println/2, calculateSquaredDistance/2]).
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
	S = #wirelessCardServerState{entities = [], nodes = initNodes(Nodes), nodesMap = Nodes},
	loop(S).

%% ====================================================================
%% Server loop
%% ====================================================================

loop(S) ->
	receive 
		{Pid, {register, Type, Pos}} 				->  
														io:format("ADD: [PID ~w | TYPE ~w | POS~w]~n", [Pid, Type, Pos]),
								  		   				NewState = registerNewEntity(S, Pid, Type, Pos),
								           				loop(NewState);
		{Pid, {setPosition, NewPos}}   	    		->  
														NewState = updateEntityPosition(S, Pid, NewPos),
									       			    loop(NewState);
		{Pid, {getPosition}}            			->  
														{Ent, EntityIsRegistered} = getEntityFromEntities(S#wirelessCardServerState.entities, Pid),
														Pos =   if EntityIsRegistered == ok -> 
																			Ent#entity.position;
																		true -> 
																			""
																end,
										   			    Pid ! Pos,
									   					loop(S);
		{printState}			 	    			->  
														my_util:println("Wireless Server State", S),
									       				loop(S);
		{Pid, {removeEntity}}   	    			->  
														NewState = removeEntity(S, Pid), 
									       				loop(NewState);
		{Pid, {getNearEntities, CurrentPos, Power}} -> 
														Results = getNearEntities(S, CurrentPos, Power),
														MappedResults = mapResultsToPidsList(Results),
														ClearedResults = lists:delete(Pid, MappedResults),
														Pid ! ClearedResults,
														loop(S);
		{Pid, {getNearCars, CurrentPos, Power}} -> 
														Results = getNearEntities(S, CurrentPos, Power),
														FilteredResults = filterResultsByType(Results, car),
														MappedResults = mapResultsToPidsList(FilteredResults),
														ClearedResults = lists:delete(Pid, MappedResults),
														Pid ! ClearedResults,
														loop(S);
		{Pid, {getNearestCar, CurrentPos}}			-> 
														Results = getEntitiesSortedByDistance(S, CurrentPos),
														FilteredResults = filterResultsByType(Results, car),
														MappedResults = mapResultsToPidsList(FilteredResults),
														ClearedResults = lists:delete(Pid, MappedResults),
														Pid ! hd(ClearedResults),
														loop(S);
		{Pid, Ref, terminate}						->
														send_message(Pid, {Ref, ok}),
														println("Exiting wireless server loop~n");
        Unknown ->
														io:format("Unknown message reseived by Wireless Server: ~p~n", [Unknown]),
														loop(S)
	end.

%% ====================================================================
%% Register and Update Positions
%% ====================================================================

% Register the new entity. If it is already there updates position
registerNewEntity(S, Pid, Type, Pos) -> 
	{_Ent, EntityIsRegistered} = getEntityFromEntities(S#wirelessCardServerState.entities, Pid),
	FinalState =  if EntityIsRegistered == ok -> 
						updateEntityPosition(S, Pid, Pos);
					true -> 
						S1 = insertEntityInPosition(S, Pos, {Pid, Type}),
						NewEntity = #entity{pid = Pid, type = Type, position = Pos},	
						NewEntities = S#wirelessCardServerState.entities ++ [NewEntity],
						S2 = S1#wirelessCardServerState{entities = NewEntities},
						S2
				end,
	FinalState.

insertEntityInPosition(State, NodeName, NodeEnt) ->
	CurrentNodesData = State#wirelessCardServerState.nodes,
	F = fun(Node) ->
			Name = Node#nodeEntities.nodeData#node.name,
			EntitiesInNode = Node#nodeEntities.entities,
			NewEntities = if Name =:= NodeName -> EntitiesInNode ++ [NodeEnt];
							true -> EntitiesInNode
						end,
			Node#nodeEntities{entities = NewEntities}
		end,
	NewNodesData = lists:map(F, CurrentNodesData),
	State#wirelessCardServerState{nodes = NewNodesData}.

updateEntityPosition(State, Pid, NewNodeName) ->
	CurrentEntities = State#wirelessCardServerState.entities,
	{Ent, EntityIsRegistered} = getEntityFromEntities(CurrentEntities, Pid),

	FinalState = if EntityIsRegistered == ok -> 
						% rimuovere il pid da dov'è se l'entità è già presente
						{S1, NodeEnt} = removeEntityFromPosition(State, Ent#entity.position, Pid),
						% metterlo nella lista nel nuovo nodo
						S2 = insertEntityInPosition(S1, NewNodeName, NodeEnt),
						% aggiornare la sua posizione nella hash

						F = fun(Entity) ->
							E_Pid = Entity#entity.pid,
							Position = Entity#entity.position,
							NewPosition = if E_Pid =:= Pid -> NewNodeName;
												true -> Position
											end,
							Entity#entity{position = NewPosition}
						end,

						NewEntities= lists:map(F, CurrentEntities),
						NewState = S2#wirelessCardServerState{
							entities = NewEntities	
						},
						NewState;
					true -> 
						State
				end,
	FinalState.

removeEntityFromPosition(State, NodeName, Pid) -> 
	CurrentNodesData = State#wirelessCardServerState.nodes,
	F = fun(Node) ->
			Name = Node#nodeEntities.nodeData#node.name,
			EntitiesInNode = Node#nodeEntities.entities,
			F = fun(X) -> {Pid_Ent, _Type} = X, 
						not (Pid == Pid_Ent)
			end,
			NewEntities = if Name =:= NodeName -> lists:filter(F, EntitiesInNode);
							true -> EntitiesInNode
						end,
			Node#nodeEntities{entities = NewEntities}
		end,
	NewNodesData = lists:map(F, CurrentNodesData),
	State#wirelessCardServerState{nodes = NewNodesData}.

removeEntity(State, Pid) ->
	CurrentEntities = State#wirelessCardServerState.entities,
	{Ent, EntityIsRegistered} = getEntityFromEntities(CurrentEntities, Pid),
	NewState =  if EntityIsRegistered == ok -> 
						S1 = removeEntityFromPosition(State, Ent#entity.position, Pid),
							Filter = fun(N) -> 
											N_Pid = N#entity.pid,
											not (Pid == N_Pid)
											end,
							NewEntities = lists:filter(Filter, CurrentEntities),
							S1#wirelessCardServerState{entities = NewEntities};
					true -> 
						State
				end,
	NewState.

%% ====================================================================
%% Near Entities Functions
%% ====================================================================

getEntitiesSortedByDistance(State, CurrentPosName) ->
	Nodes = State#wirelessCardServerState.nodesMap,
	{StartNode_X, StartNode_Y} = nodes_util:getPositionFromNodeName(CurrentPosName, Nodes),
	NodesWireless = State#wirelessCardServerState.nodes,
	
	MapFunc = fun(Node) ->	  NodeData = Node#nodeEntities.nodeData,
								Curr_X = NodeData#node.pos_x,
								Curr_Y = NodeData#node.pos_y,
								SquaredDistanceNodes = calculateSquaredDistance({StartNode_X, StartNode_Y}, {Curr_X, Curr_Y}),
								#nodeDistance{dist = SquaredDistanceNodes,
											entities = Node#nodeEntities.entities
											}
							end,
	EntitiesDistances = lists:map(MapFunc, NodesWireless),	
	SortFun = fun(A, B) -> 
						A#nodeDistance.dist < B#nodeDistance.dist
						end,
	Sorted_Nodes = lists:sort(SortFun, EntitiesDistances),
	PackedEntities = packNodesEntities(Sorted_Nodes, []),
	PackedEntities.

getNearEntities(State, CurrentPosName, Max_Distance) ->
	Nodes = State#wirelessCardServerState.nodesMap,
	{StartNode_X, StartNode_Y} = nodes_util:getPositionFromNodeName(CurrentPosName, Nodes),
	SquaredMaxRange = Max_Distance * Max_Distance,
	
	NodesWireless = State#wirelessCardServerState.nodes,
	FilterFunc = fun(Node) -> %CurrentNodeName = X#nodeEntities.nodeData#node.name,
								NodeData = Node#nodeEntities.nodeData,
								Curr_X = NodeData#node.pos_x,
								Curr_Y = NodeData#node.pos_y,
								SquaredDistanceNodes = calculateSquaredDistance({StartNode_X, StartNode_Y}, {Curr_X, Curr_Y}),
								SquaredDistanceNodes < SquaredMaxRange
							end,
	NearNodes = lists:filter(FilterFunc, NodesWireless),
	MapFunc = fun(Node) -> #nodeDistance{dist = 0,
								entities = Node#nodeEntities.entities
							} end,

	MappedNodes = lists:map(MapFunc, NearNodes),

	PackedEntities = packNodesEntities(MappedNodes, []),
	PackedEntities.
	
packNodesEntities([], ACC) -> ACC;	
packNodesEntities([H | T ], ACC) -> 
	NewACC = ACC ++ H#nodeDistance.entities,
	packNodesEntities(T, NewACC).
	
%% ====================================================================
%% Utils Functions
%% ====================================================================
initNodes(Nodes) -> initNode(Nodes, []).

initNode([], ACC) -> ACC;
initNode([H | T], ACC) -> 
	StateNode = #nodeEntities{nodeData = H,
							entities = []},
	NewACC = [StateNode] ++ ACC,
	initNode(T, NewACC).

getEntityFromEntities(Entities, ToSearch) ->
	Out = lists:filter(fun(X) -> (X#entity.pid == ToSearch) end, Entities),
	if Out =:= [] -> {[], none};
		true -> 
			El = hd(Out),
			{El, ok}
	end.

% Get List in form of [{PID, type}, ...]
filterResultsByType(Results, Type) ->
	FilterFunc = fun(EL) ->
			{_Pid_Ent, EL_Type} = EL,
			EL_Type == Type 
		end,
	FilteredResults = lists:filter(FilterFunc, Results),
	FilteredResults.

mapResultsToPidsList(Results) ->
	MapFunc = fun(EL) ->
			{Pid_Ent, _EL_Type} = EL,
			Pid_Ent
		end,
	MappedResults = lists:map(MapFunc, Results),
	MappedResults.

%% ====================================================================
%% Various Functions
%% ====================================================================

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
