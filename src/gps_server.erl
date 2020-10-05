%% @author Alessandro
%% @doc @todo Add description to gps_server.

-module(gps_server).

-include("records.hrl").
-include("globals.hrl").

-import('my_util',[println/1, println/2, calculateSquaredDistance/2]).
-import('send', [send_message/2, send_message/3]).

-record(nodeDistance,{dist, entities}).
-record(entity, {pid, type, position}). 
-record(nodeEntities, {nodeData, entities}).
-record(gpsServerState, {entities, nodes}).

-export([start_gps_server/1, init/1, end_gps_server/1, 
		sendPosToGps/2, deleteMyLocationTracks/1, getNearestCar/1, printInternalState/1]).

%% ====================================================================
%% API functions
%% ====================================================================

%Testing Code

sendPosToGps(PID_GPS_Server, Position) ->
	PID_GPS_Server ! {self(), {setPosition, Position}}.

deleteMyLocationTracks(PID_GPS_Server) ->
	PID_GPS_Server ! {self(), {removeEntity}}.

%torna macchina più vicina
getNearestCar(PID_GPS_Server) ->
	PID_GPS_Server ! {self(), {getNearestCar, self()}}.

%for debugging porp.
printInternalState(PID_GPS_Server) ->
	PID_GPS_Server ! {printState}.
	
start_gps_server(Nodes) -> 
	spawn_link('gps_server', init, [Nodes]).

end_gps_server(Pid) ->
	println("Killing GPS Server"),
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

%% ====================================================================
%% Init Functions
%% ====================================================================

init(Nodes) -> 
	S = #gpsServerState{entities = [], nodes = initNodes(Nodes)},
	loop(S).

initNodes(Nodes) -> initNode(Nodes, []).

initNode([], ACC) -> ACC;
initNode([H | T], ACC) -> 
	StateNode = #nodeEntities{nodeData = H,
							entities = []},
	NewACC = [StateNode] ++ ACC,
	initNode(T, NewACC).

%% ====================================================================
%% Server loop
%% ====================================================================

loop(S) ->
	receive 
		{Pid, {register, Type, Pos}} 				->  
														io:format("ADD: [PID ~w | TYPE ~w | POS ~w]~n", [Pid, Type, Pos]),
								  		   				NewState = registerNewEntity(S, Pid, Type, Pos),
								           				loop(NewState);
		{Pid, {setPosition, NewPos}}   	    		->  
														NewState = updateEntityPosition(S, Pid, NewPos),
									       			    loop(NewState);
		{Pid, {getPosition}}            			->  
														{Ent, EntityIsRegistered} = getEntityFromEntities(S#gpsServerState.entities, Pid),
														Pos =   if EntityIsRegistered == ok -> 
																			Ent#entity.position;
																		true -> 
																			""
																end,
														send_message(Pid, Pos),
									   					loop(S);
		{printState}			 	    			->  
														my_util:println("GPS Server State", S),
									       				loop(S);
		{Pid, {removeEntity}}   	    			->  
														{Ent, EntityIsRegistered} = getEntityFromEntities(S#gpsServerState.entities, Pid),
														S1 =   if EntityIsRegistered == ok -> 
																		removeEntityFromPosition(S, Ent#entity.position, Pid);
																		true -> 
																			S
																end,
														NewState = removeEntity(S1, Pid), 
									       				loop(NewState);
		{Pid, {getNearEntities, CurrentPos, Power}} -> 
														Results = getNearEntities(S, CurrentPos, Power),
														send_message(Pid, self(), Results),
														loop(S);
		{Pid, {getSortedEntities, CurrentPos, Power}} -> 
														Results = getEntitiesSortedByDistance(S, CurrentPos, Power),
														send_message(Pid, self(), Results),
														loop(S);
		{Pid, Ref, terminate}						->
														send_message(Pid, {Ref, ok}),
														println("Exiting gps server loop~n");
        Unknown ->
														io:format("Unknown message reseived by Gps Server: ~p~n", [Unknown]),
														loop(S)
	end.

%% ====================================================================
%% Register and Update Positions
%% ====================================================================

% Register the new entity. If it is already there updates position
registerNewEntity(S, Pid, Type, Pos) -> 
	{_Ent, EntityIsRegistered} = getEntityFromEntities(S#gpsServerState.entities, Pid),
	FinalState =  if EntityIsRegistered == ok -> 
						updateEntityPosition(S, Pid, Pos);
					true -> 
						S1 = insertEntityInPosition(S, Pos, {Pid, Type}),
						NewEntity = #entity{pid = Pid, type = Type, position = Pos},	
						NewEntities = S#gpsServerState.entities ++ [NewEntity],
						S2 = S1#gpsServerState{entities = NewEntities},
						S2
				end,
	FinalState.

insertEntityInPosition(State, NodeName, NodeEnt) ->
	CurrentNodesData = State#gpsServerState.nodes,
	F = fun(Node) ->
			Name = Node#nodeEntities.nodeData#node.name,
			EntitiesInNode = Node#nodeEntities.entities,
			NewEntities = if Name =:= NodeName -> EntitiesInNode ++ [NodeEnt];
							true -> EntitiesInNode
						end,
			Node#nodeEntities{entities = NewEntities}
		end,
	NewNodesData = lists:map(F, CurrentNodesData),
	State#gpsServerState{nodes = NewNodesData}.

updateEntityPosition(State, Pid, NewNodeName) ->
	CurrentEntities = State#gpsServerState.entities,
	{Ent, EntityIsRegistered} = getEntityFromEntities(CurrentEntities, Pid),

	FinalState = if EntityIsRegistered == ok -> 
						% rimuovere il pid da dov'è se l'entità è già presente
						S1 = removeEntityFromPosition(State, Ent#entity.position, Pid),
						% metterlo nella lista nel nuovo nodo
						NodeEnt = {Pid, Ent#entity.type},
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
						NewState = S2#gpsServerState{
							entities = NewEntities	
						},
						NewState;
					true -> 
						State
				end,
	FinalState.

removeEntityFromPosition(State, NodeName, Pid) -> 
	CurrentNodesData = State#gpsServerState.nodes,
	F = fun(Node) ->
			Name = Node#nodeEntities.nodeData#node.name,
			EntitiesInNode = Node#nodeEntities.entities,
			F = fun(X) -> {Pid_Ent, _Type} = X, 
						not (Pid == Pid_Ent)
				end,
			NewEntities = if Name =:= NodeName -> 
								 	lists:filter(F, EntitiesInNode);
								true -> 
									EntitiesInNode
							end,
			Node#nodeEntities{entities = NewEntities}
		end,
	NewNodesData = lists:map(F, CurrentNodesData),
	State#gpsServerState{nodes = NewNodesData}.

removeEntity(State, Pid) ->
	CurrentEntities = State#gpsServerState.entities,
	{Ent, EntityIsRegistered} = getEntityFromEntities(CurrentEntities, Pid),
	NewState =  if EntityIsRegistered == ok -> 
						S1 = removeEntityFromPosition(State, Ent#entity.position, Pid),
							Filter = fun(N) -> 
											N_Pid = N#entity.pid,
											not (Pid == N_Pid)
											end,
							NewEntities = lists:filter(Filter, CurrentEntities),
							S1#gpsServerState{entities = NewEntities};
					true -> 
						State
				end,
	NewState.

%% ====================================================================
%% Near Entities Functions
%% ====================================================================

calculateNodesDistances(State, CurrentPosName) ->
	GPS_Nodes = State#gpsServerState.nodes,
	{StartNode_X, StartNode_Y} = getPositionFromNodes(CurrentPosName, GPS_Nodes),
	
	MapFunc = fun(Node) ->	  
				NodeData = Node#nodeEntities.nodeData,
				Curr_X = NodeData#node.pos_x,
				Curr_Y = NodeData#node.pos_y,
				SquaredDistanceNodes = calculateSquaredDistance({StartNode_X, StartNode_Y}, {Curr_X, Curr_Y}),
				#nodeDistance{dist = SquaredDistanceNodes,
							entities = Node#nodeEntities.entities
							}
			end,

	EntitiesDistances = lists:map(MapFunc, GPS_Nodes),	
	EntitiesDistances.

filterNodesByDistance(NodesDistances, Max_Distance) ->
	SquaredDistance = Max_Distance * Max_Distance,
	FilterFunc = fun(Node) -> Node#nodeDistance.dist < SquaredDistance end,
	FilteredNodes = lists:filter(FilterFunc, NodesDistances),
	FilteredNodes.

getEntitiesSortedByDistance(State, CurrentPosName, Max_Distance) ->
	NodesDistances = calculateNodesDistances(State, CurrentPosName),
	FilteredNodes = filterNodesByDistance(NodesDistances, Max_Distance),

	SortFun = fun(A, B) -> 
						A#nodeDistance.dist < B#nodeDistance.dist
						end,
	Sorted_Nodes = lists:sort(SortFun, FilteredNodes),

	PackedEntities = packNodesEntities(Sorted_Nodes, []),
	PackedEntities.

getNearEntities(State, CurrentPosName, Max_Distance) ->
	NodesDistances = calculateNodesDistances(State, CurrentPosName),
	FilteredNodes = filterNodesByDistance(NodesDistances, Max_Distance),
	PackedEntities = packNodesEntities(FilteredNodes, []),
	PackedEntities.
	
%% ====================================================================
%% Utils Functions
%% ====================================================================
getPositionFromNodes(NodeName, GPS_Nodes) ->
	getPos(NodeName, GPS_Nodes).

getPos(_NodeName, []) -> {-1, -1};
getPos(NodeName, [H | T]) -> 
	Name = H#nodeEntities.nodeData#node.name,
	PosX = H#nodeEntities.nodeData#node.pos_x,
	PosY = H#nodeEntities.nodeData#node.pos_y,
	if NodeName =:= Name -> {PosX, PosY};
	   true -> getPos(NodeName, T)
	end.

getEntityFromEntities(Entities, ToSearch) ->
	Out = lists:filter(fun(X) -> (X#entity.pid == ToSearch) end, Entities),
	if Out =:= [] -> {[], none};
		true -> 
			El = hd(Out),
			{El, ok}
	end.

packNodesEntities([], ACC) -> ACC;	
packNodesEntities([H | T ], ACC) -> 
	NewACC = ACC ++ H#nodeDistance.entities,
	packNodesEntities(T, NewACC).

