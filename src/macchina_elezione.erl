-module(macchina_elezione).
-compile(export_all).
-import('send', [send_message/2, send_message/3]).
-behaviour(gen_statem).
-include("records.hrl").
-include("globals.hrl").

callback_mode() -> [state_functions].

-record(carPartecipate, {refCar, costCar, sentResults}).

-record(electionState, {
				currentMovingQueue,
				pidCar,
				pidGps,
				pidAppUser,
				parent,
				carsInvited,
				childrenPartecipate,
				childrenResponded,
				selfCost,
				childrenCosts,
				cityMap,
				tick_counter,
				currentRequest,
				flag_initiator,
				dijkstra_results,
				battery_level}).

-record(dataElectionBegin,{currentQueue,
							request,
							pidAppUser}).

-record(dataElectionPartecipate, {
				pidParent,
				request, %la tupla {From,To}
				ttl}).
%% ====================================================================
%% API functions
%% ====================================================================

start(PidMacchina,PidWifi,City_Map) ->
	State = #electionState {
							pidAppUser = none,
							currentMovingQueue = [],
							pidCar = PidMacchina,
							pidGps = PidWifi,
							parent = none,
							carsInvited = [],
							childrenPartecipate = [],
							childrenResponded = [],
							selfCost = {-1 , -1},
							childrenCosts = [],
							cityMap = City_Map,
						   	tick_counter = 0,
							currentRequest = {},
							flag_initiator = false,
							dijkstra_results = [],
							battery_level = 0},
	{ok, Pid} = gen_statem:start_link(?MODULE,State, []),
	Pid.
	
%% ====================================================================
%% Automata Functions
%% ====================================================================


init(State) ->
	{ok, idle, State}.

idle(info, {_From, tick}, _Stato) ->
	keep_state_and_data; %per ora non fare nulla

idle(cast, {beginElection, Data}, S) ->
	% Update State
	S1 = S #electionState { currentRequest = Data#dataElectionBegin.request,
							currentMovingQueue = Data#dataElectionBegin.currentQueue,
							pidAppUser =  Data#dataElectionBegin.pidAppUser},
							
%%% Recupero dati da altri automi della macchina
	PID_GPS = Data#electionState.pidGps,
	send_message(PID_GPS, {getNearCars}),
	CloserCars = receive
		Cars -> Cars
	end,
	send_message(PID_GPS, {getPosition}),
	CurrentPosition = receive
		Pos -> Pos
	end,
%%------------------------------------------------

	DataPartecipate = #dataElectionPartecipate{
								pidParent = S1#electionState.pidCar,
								request = S1#electionState.currentRequest,
								ttl = 2},

	%invio partecipateElection ai vicini
	sendMessage(CloserCars, DataPartecipate),
										
	{From, To} = S #electionState.currentRequest,
	NearestColumnTarget = calculateNearestColumn(To),
	Dijkstra_Results = calculateDijkstra(CurrentPosition, From, To, NearestColumnTarget),
	Battery_level = macchina_moving:getBatteryLevel(S1#electionState.pidCar),
	{CC, CRDT} = calculateSelfCost(Dijkstra_Results, Battery_level),

	S2 = S1#electionState { 
		flag_initiator = true,
		carsInvited = CloserCars,
		selfCost = {CC, CRDT}
	},

	{next_state, running, S2};
		
idle(cast, {partecipateElection, Data}, S) ->
	% Update State
	S1 = S #electionState { currentRequest = Data#dataElectionPartecipate.request,
							parent = Data#dataElectionPartecipate.pidParent},

	CurrentTTL = Data#dataElectionPartecipate.pidParent,
%%% Recupero dati da altri automi della macchina
	PID_GPS = Data#electionState.pidGps,
	CloserCars_All = 
		if CurrentTTL > 0 -> 
			send_message(PID_GPS, {getNearCars}),
			receive
				Cars -> Cars
			end;
			true -> []
		end,
	send_message(PID_GPS, {getPosition}),
	CurrentPosition = receive
		Pos -> Pos
		end,
%%------------------------------------------------
	
	CloserCars = lists:delete(S1#electionState.pidCar, CloserCars_All),
	DataPartecipate = #dataElectionPartecipate{
								pidParent = S1#electionState.pidCar,
								request = S1#electionState.currentRequest,
								ttl = CurrentTTL - 1},

	%invio partecipateElection ai vicini
	sendMessage(CloserCars, DataPartecipate),
	sendMessageElection(S#electionState.parent, {invite_result, {S1#electionState.pidCar, i_can_join}}),

% Calcolo dei miei costi
	{From, To} = S #electionState.currentRequest,
	NearestColumnTarget = calculateNearestColumn(To),
	Dijkstra_Results = calculateDijkstra(CurrentPosition, From, To, NearestColumnTarget),
	Battery_level = macchina_moving:getBatteryLevel(S1#electionState.pidCar),
	{CC, CRDT} = calculateSelfCost(Dijkstra_Results, Battery_level),
%%------------------------------------------------

	S2 = S1#electionState { 
		carsInvited = CloserCars,
		selfCost = {CC, CRDT}
	},

	{next_state, running, S2}.

running(cast, {invite_result, _Data}, S) -> {next_state, running, S};
running(cast, {costs_results, _Data}, S) -> {next_state, running, S}.

%running
%-> wait_results
%-> calculate_results
%{election_data, Data}
%Data = {election_results, {id_winner}} id_winner è da confrontare con IDCar
%Data = {invite_result, {pidCar, i_can_join}}
%Data = {invite_result, {pidCar, i_can_not_join}}
%Data = {costs_results, [{pid_source, cost_client, charge_after_transport}]}

%% ====================================================================
%% Internal functions
%% ====================================================================
calculateSelfCost(_Data, Battery_level) -> 
	io:format("Battery level: ~w~n", [Battery_level]),
	{my_util:generate_random_number(100), my_util:generate_random_number(100)}.

calculateNearestColumn(_Node) -> "".

calculateDijkstra(_CurrentPosition, _From, _To, _NearestColumnTarget) -> [].

sendMessageElection(Target, Data) ->
	send_message(Target, {election_data, Data}).

sendMessage([], _Data) -> sendedAll;
sendMessage([H | T], Data) -> 
	sendMessageElection(H, Data),
	sendMessage(T, Data).

sendDebugMessage([], _Data) -> sendedAll;
sendDebugMessage([H | T], Data) -> 
	gen_statem:cast(H, Data),
	sendDebugMessage(T, Data).