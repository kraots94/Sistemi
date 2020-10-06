-module(macchina_elezione).
-compile(export_all).
-import('send', [send_message/2, send_message/3]).
-behaviour(gen_statem).
-include("records.hrl").
-include("globals.hrl").

callback_mode() -> [state_functions].

-record(carPartecipate, {refCar, costsCar, sentResults}).
-record(electionCostData, {pid_source, cost_client, charge_after_transport}).

-record(electionState, {
				currentMovingQueue,
				pidCar,
				pidMovingCar,
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
				battery_level,
				total_tree_costs}).

%% ====================================================================
%% API functions
%% ====================================================================

start(PidMacchina,PidMovingCar, PidWifi,City_Map) ->
	State = #electionState {
							pidAppUser = none,
							currentMovingQueue = [],
							pidCar = PidMacchina,
							pidMovingCar = PidMovingCar,
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
							battery_level = 0,
							total_tree_costs = []},

	{ok, Pid} = gen_statem:start_link(?MODULE,State, []),
	Pid.
	
resetState(S) ->
	S#electionState {
					pidAppUser = none,
					currentMovingQueue = [],
					parent = none,
					carsInvited = [],
					childrenPartecipate = [],
					childrenResponded = [],
					selfCost = {-1 , -1},
					childrenCosts = [],
					tick_counter = 0,
					currentRequest = {},
					flag_initiator = false,
					dijkstra_results = [],
					battery_level = 0,
					total_tree_costs = []}.
%% ====================================================================
%% Automata Functions
%% ====================================================================


init(State) ->
	{ok, idle, State}.

idle(info, {_From, tick}, _Stato) ->
	keep_state_and_data; %per ora non fare nulla

idle(cast, {beginElection, Data}, S) ->
	% Update State
	S1 = S#electionState { currentRequest = Data#dataElectionBegin.request,
							pidAppUser =  Data#dataElectionBegin.pidAppUser},

%%% Recupero dati da altri automi della macchina
	PID_GPS = S1#electionState.pidGps,
	CloserCars = getDataFromAutomata(PID_GPS, {self(), getNearCars}),

	%{Cost_To_Last_Target, Current_Last_Target, Battery_Level} = getDataFromAutomata(PID_MOV, {getDataElection}),
	Battery_level = 200000,
	Cost_To_Last_Target = 0,
	Current_Last_Target = "hmmsii",
%%------------------------------------------------

	DataPartecipate = #dataElectionPartecipate{
								pidParent = S1#electionState.pidCar,
								request = S1#electionState.currentRequest,
								ttl = 2},

	%invio partecipateElection ai vicini
	sendInvitesPartecipation(CloserCars, DataPartecipate, S),
										
	From = S1#electionState.currentRequest#user_request.from,
	To = S1#electionState.currentRequest#user_request.to,

	% Calcolo dei miei costi
	From = S1#electionState.currentRequest#user_request.from,
	To = S1#electionState.currentRequest#user_request.to,
	NearestColumnTarget = calculateNearestColumn(To),
	Dijkstra_Results = calculateDijkstra(Current_Last_Target, From, To, NearestColumnTarget),
	{CC, CRDT} = calculateSelfCost(Dijkstra_Results, Battery_level, Cost_To_Last_Target),
	%%------------------------------------------------

	S2 = S1#electionState { 
		flag_initiator = true,
		carsInvited = CloserCars,
		selfCost = {CC, CRDT}
	},

	if S2#electionState.carsInvited =:= [] -> 
		{Cost_Client, Charge_After_Client} = S2#electionState.selfCost,
		Results = #electionCostData{
						pid_source = S2#electionState.pidCar, 
						cost_client = Cost_Client, 
						charge_after_transport = Charge_After_Client},
		%aspetto i risultati
		calculate_final_results([Results], S2),
		{next_state, idle, S2};
	true ->  
		{next_state, running_election, S2} 
	end;

idle(cast, {partecipateElection, Data}, S) ->
	% Update State
	S1 = S#electionState{currentRequest = Data#dataElectionPartecipate.request,
	parent = Data#dataElectionPartecipate.pidParent},

	CurrentTTL = Data#dataElectionPartecipate.ttl,

	%%% Recupero dati da altri automi della macchina
	PID_GPS = S1#electionState.pidGps,
	_PID_MOV = -1,
	CloserCars_All = if CurrentTTL > 0 -> 
							getDataFromAutomata(PID_GPS, {self(), getNearCars}); 
						true -> 
							[] 
					end,

	%{Cost_To_Last_Target, Current_Last_Target, Battery_Level} = getDataFromAutomata(PID_MOV, {getDataElection}),
	Battery_level = 200000,
	Cost_To_Last_Target = 0,
	Current_Last_Target = "hmmsii",
	%%------------------------------------------------	

	CloserCars = lists:delete(S1#electionState.pidCar, CloserCars_All),
	DataPartecipate = #dataElectionPartecipate{
								pidParent = S1#electionState.pidCar,
								request = S1#electionState.currentRequest,
								ttl = CurrentTTL - 1},

	%invio partecipateElection ai vicini
	sendInvitesPartecipation(CloserCars, DataPartecipate, S1),
	Target = S1#electionState.parent,
	sendMessageElection(Target, {invite_result, {S1#electionState.pidCar, i_can_join}}, S1),

% Calcolo dei miei costi
	From = S1#electionState.currentRequest#user_request.from,
	To = S1#electionState.currentRequest#user_request.to,
	NearestColumnTarget = calculateNearestColumn(To),
	Dijkstra_Results = calculateDijkstra(Current_Last_Target, From, To, NearestColumnTarget),
	{CC, CRDT} = calculateSelfCost(Dijkstra_Results, Battery_level, Cost_To_Last_Target),
%%------------------------------------------------

	S2 = S1#electionState { 
		carsInvited = CloserCars,
		selfCost = {CC, CRDT}
	},

	if S#electionState.carsInvited =:= [] -> 
			%mando i dati a mio padre
			{Cost_Client, Charge_After_Client} = S2#electionState.selfCost,
			Results = #electionCostData{
							pid_source = S2#electionState.pidCar, 
							cost_client = Cost_Client, 
							charge_after_transport = Charge_After_Client},

			sendMessageElection(S2#electionState.parent, {costs_results, {S2#electionState.pidCar, [Results]}}, S2),
			%aspetto i risultati
			{next_state, waiting_final_results, S2};
		true ->  
			{next_state, running_election, S2} 
	end.

running_election(info, {_From, tick}, _Stato) ->
	keep_state_and_data; %per ora non fare nulla

running_election(cast, {invite_result, Data}, S) -> 
	{PID_Car, Partecipate} = Data,
	NewInvited = lists:delete(PID_Car, S#electionState.carsInvited),
	OldChildrenPartecipate = S#electionState.childrenPartecipate,
	NewChildrenPartecipate =  
				if Partecipate == i_can_join 
						-> {_El, State} = searchPartecipantInList(OldChildrenPartecipate, PID_Car),
							if State == ok -> OldChildrenPartecipate;
									true -> 
										NewPartecipant = #carPartecipate{refCar = PID_Car, costsCar = [], sentResults = not_sended_results},
										[NewPartecipant] ++ OldChildrenPartecipate
							end;	
					true -> OldChildrenPartecipate
				end,
	{next_state, running_election, S#electionState{carsInvited = NewInvited, 
													childrenPartecipate = NewChildrenPartecipate}};

running_election(cast, {costs_results, Data}, S) -> 
	{PID_Sender , Results} = Data,
	CurrentPartecipants = S#electionState.childrenPartecipate,
	UserAlreadyInInvited = lists:member(PID_Sender,  S#electionState.carsInvited),
	NewInvited = lists:delete(PID_Sender, S#electionState.carsInvited),
	NewPartecipants = if
		UserAlreadyInInvited ->
			NewPartecipant = #carPartecipate{
				refCar = PID_Sender,
				sentResults = ok, 
				costsCar = Results
			},
			CurrentPartecipants ++ [NewPartecipant];
		true ->
			FuncMap = fun(Partecipant) -> 
				SentResults = Partecipant#carPartecipate.sentResults,
				NewPartecipant = if
					SentResults == ok -> 
						Partecipant;					
					true ->
						PID_Partecipant = Partecipant#carPartecipate.refCar,
						if PID_Sender =:= PID_Partecipant -> 
							Partecipant#carPartecipate{
								sentResults = sended_results, 
								costsCar = Results
							};
						true ->
							Partecipant
					end
				end,
				NewPartecipant
			end,
			lists:map(FuncMap, CurrentPartecipants)
	end,

	S1 = S#electionState{carsInvited = NewInvited, 
						childrenPartecipate = NewPartecipants},

	FuncFilter = fun(Partecipant) -> Partecipant#carPartecipate.sentResults == not_sended_results end,
	MissingAnswers = lists:filter(FuncFilter, NewPartecipants),
	if  (MissingAnswers =:= []) and (NewInvited =:= []) -> 
			ChildrenCosts = packChildrenCosts(NewPartecipants, []),
			{Cost_Client, Charge_After_Client} = S1#electionState.selfCost,
			My_Results = #electionCostData{
							pid_source = S1#electionState.pidCar, 
							cost_client = Cost_Client, 
							charge_after_transport = Charge_After_Client},
			TotalCosts = [My_Results] ++ ChildrenCosts,
			if S1#electionState.flag_initiator -> 
					calculate_final_results(TotalCosts, S1),
					S2 = resetState(S1),
					{next_state, idle, S2};
				true -> 					
					sendMessageElection(S1#electionState.parent, {costs_results, {S1#electionState.pidCar, [Results]}}, S1),
				{next_state, waiting_final_results, S1} 
			end;
		true -> 
			{next_state, running_election, S1}	
	end.

calculate_final_results(DataTree, S) -> 
	io:format("Automa Election - calculate_final_results ~w~n", [S#electionState.pidCar]),
	SortFun = fun(A, B) -> 
		CostClient_A = A#electionCostData.cost_client, 
		Final_Charge_A = A#electionCostData.charge_after_transport,  
		CostClient_B = B#electionCostData.cost_client, 
		Final_Charge_B  = B#electionCostData.charge_after_transport, 
		if CostClient_A == CostClient_B -> Final_Charge_A > Final_Charge_B;
				true -> CostClient_A < CostClient_B
		end
	end,
	Sorted_Data = lists:sort(SortFun, DataTree),
	io:format("Sorted_Data: ~w~n", [Sorted_Data]),
	Winner_Partecipant = hd(Sorted_Data),
	Winner_Data = #election_result_to_car{id_winner = Winner_Partecipant#electionCostData.pid_source,
										id_app_user = S#electionState.pidAppUser
										},
	ID_Winner = Winner_Data#election_result_to_car.id_winner,
	My_Pid = S#electionState.pidCar,
	if
		My_Pid == ID_Winner ->
			_ID_APP_User = Winner_Data#election_result_to_car.id_app_user,
			% creo i record per la coda
			% aggiorno la coda movement all'automa
			io:format("Election Automata, I Won: [~w] [~w] ~n", [Winner_Data, My_Pid]);
		true ->
			ok
	end,
	FuncMap_2 = fun(Child) -> Child#carPartecipate.refCar end,
	Pids_To_notify = lists:map(FuncMap_2, S#electionState.childrenPartecipate),

	sendMessage(Pids_To_notify, {winning_results, Winner_Data}, S),
	sendToListener({election_results, Winner_Data}, S).

waiting_final_results(info, {_From, tick}, _Stato) ->
	keep_state_and_data; %per ora non fare nulla

waiting_final_results(cast, {winning_results, Data}, S) -> 
	ID_Winner = Data#election_result_to_car.id_winner,
	My_Pid = S#electionState.pidCar,
	if
		My_Pid == ID_Winner ->
			_ID_APP_User = Data#election_result_to_car.id_app_user,
			% creo i record per la coda
			% aggiorno la coda movement all'automa
			io:format("Election Automata, I Won: [~w] [~w] ~n", [Data, My_Pid]);
		true ->
			ok
	end,
	FuncMap_2 = fun(Child) -> Child#carPartecipate.refCar end,
	Pids_To_notify = lists:map(FuncMap_2, S#electionState.childrenPartecipate),
	sendMessage(Pids_To_notify, {winning_results, Data}, S),
	sendToListener({election_results, Data}, S),
	S1 = resetState(S),
	{next_state, idle, S1};

waiting_final_results(cast, {invite_result, _Data}, S) -> 
	{next_state, waiting_final_results, S}.
 
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
calculateSelfCost(_Data, _Battery_level, _Cost_To_Last_Target) -> 
	{my_util:generate_random_number(100), my_util:generate_random_number(100)}.

calculateNearestColumn(_Node) -> "".

calculateDijkstra(_CurrentPosition, _From, _To, _NearestColumnTarget) -> [].

sendInvitesPartecipation([], _Data, _S) -> sendedAll;
sendInvitesPartecipation([H | T], Data, S) -> 
	gen_statem:cast(S#electionState.pidCar, {to_outside,  {H, {partecipateElection, Data}}}),
	sendInvitesPartecipation(T, Data, S).

sendMessageElection(Target, Data, S) ->
	gen_statem:cast(S#electionState.pidCar, {to_outside, {Target, {election_data, Data}}}).
	
sendToListener(Data, S) ->
	gen_statem:cast(S#electionState.pidCar, Data).

sendMessage([], _Data, _S) -> sendedAll;
sendMessage([H | T], Data, S) -> 
	sendMessageElection(H, Data, S),
	sendMessage(T, Data, S).

getDataFromAutomata(PID, Request) ->
	send_message(PID, Request),
	receive
		Data -> Data
	end.

searchPartecipantInList(Partecipans, ToSearch) ->
	Out = lists:filter(fun(X) -> (X#carPartecipate.refCar == ToSearch) end, Partecipans),
	if Out =:= [] -> {[], none};
		true -> 
			El = hd(Out),
			{El, ok}
	end.


packChildrenCosts([], ACC) -> ACC;	
packChildrenCosts([H | T ], ACC) -> 
	NewACC = ACC ++ H#carPartecipate.costsCar,
	packChildrenCosts(T, NewACC).
