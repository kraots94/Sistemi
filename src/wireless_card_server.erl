-module(wireless_card_server).
-compile(export_all).

-record(entityPosition, {pid, position}). 
-record(nodeEntities, {nodeData, entities, nearNodes}).
-record(wirelessCardServerState, {
				entityPositions, %lista di entityPosition ossia di roba {Pid, {x,y}}
				nodes}).

-include("records.hrl").
-include("globals.hrl").

%f(), make:all(), PID_Wireless_Server = wireless_card_server:start_link(nodes_util:load_nodes()).

start_link(Nodes) -> spawn_link('wireless_card_server', init, [Nodes]).

init(Nodes) -> 
	S = #wirelessCardServerState{entityPositions = [], nodes = initNodes(Nodes)},
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
		{Pid, {setPosition, NewPos}} -> NewState = updateEntityPosition(S, Pid, NewPos),
									 loop(NewState);
		{Pid, {getPosition}}       ->  {Pos, _Res} = getPos(S#wirelessCardServerState.entityPositions, Pid),
										Pid ! Pos;
		{Pid, {printState}} 	-> 	my_util:println("Wireless Server State", S),
									Pid ! printed,
									 loop(S);
%		{Pid, {removePosition}}   ->  NewState = removeEntityPosition(S, Pid), loop(NewState);
		{Pid, {getNearEntities, CurrentPos, Power}} -> Pid ! getNearEntities(S, CurrentPos, Power)
%		{Pid, {getNearestCar, CurrentPos}} -> Pid ! hd(getNear(Pid, Copertura,S)) %se fai sorting del risultato di getNear sulla distanza puoi fare cosi
	end.

getNearEntities(State, CurrentPosName, Max_Distance) ->
	Nodes = State#wirelessCardServerState.nodes,
	{StartNode_X, StartNode_Y} = nodes_util:getPositionFromNodeName(CurrentPosName, Nodes),
	SquaredDistance = Max_Distance * Max_Distance,
	
	FilterFunc = fun(Node) -> %CurrentNodeName = X#nodeEntities.nodeData#node.name,
							  Curr_X = Node#nodeEntities.nodeData#node.pos_x,
							  Curr_Y = Node#nodeEntities.nodeData#node.pos_y,
							  Diff_1 = Curr_X - StartNode_X,
							  Diff_2 = Curr_Y - StartNode_Y,
							  Res = Diff_1 * Diff_1 + Diff_2 * Diff_2,
							  not (Res == 0) and Res < SquaredDistance
						 end,
	NearNodes = lists:filter(FilterFunc, Nodes),

	NodesEntities = getEntities(NearNodes, []),
	NodesEntities.
	
getEntities([], ACC) -> ACC;	
getEntities([H | T ], ACC) -> 
	NewACC = ACC ++ H#nodeEntities.entities,
	getEntities(T, NewACC).
	
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
