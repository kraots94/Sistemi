-module(macchina_elezione).
-compile(export_all).
-import('send', [send_message/2, send_message/3]).
-import('utilities', [print_debug_message/1, print_debug_message/2, print_debug_message/3]).
-behaviour(gen_statem).
-include("records.hrl").
-include("globals.hrl").

callback_mode() -> [state_functions, state_enter].

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
				total_tree_costs,
				dataToSendPartecipate,
				totalCosts}).

%% ====================================================================
%% API functions
%% ====================================================================

start(PidMacchina, PidMovingCar, Pid_Gps_Car, City_Map) ->
	State = #electionState {
							pidAppUser = none,
							currentMovingQueue = [],
							pidCar = PidMacchina,
							pidMovingCar = PidMovingCar,
							pidGps = Pid_Gps_Car,
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
							total_tree_costs = [],
							dataToSendPartecipate = none,
							totalCosts = []},

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
					total_tree_costs = [],
					dataToSendPartecipate = none,
					totalCosts = []}.
%% ====================================================================
%% Automata Functions
%% ====================================================================

init(State) ->
	{ok, idle, State}.
	
%% ====================================================================
%% Idle States functions
%% ====================================================================

idle(enter, _OldState, _State) ->
	keep_state_and_data;

idle(info, {_From, tick}, _Stato) ->
	keep_state_and_data; %per ora non fare nulla

idle(cast, {beginElection, Data}, S) ->
	%print_debug_message(S#electionState.pidCar, "Begin Election", []),
	% Update State
	S1 = S#electionState { currentRequest = Data#dataElectionBegin.request,
							pidAppUser =  Data#dataElectionBegin.pidAppUser},

%%% Recupero dati da altri automi della macchina
	PID_GPS = S1#electionState.pidGps,
	CloserCars = getDataFromAutomata(PID_GPS, {self(), getNearCars}),

	%print_debug_message(S#electionState.pidCar, "Close Cars: ~w", [CloserCars]),
	
	%{Cost_To_Last_Target, Current_Last_Target, Battery_Level} = getDataFromAutomata(PID_MOV, {getDataElection}),
	Battery_level = 200000,
	Cost_To_Last_Target = 0,
	Current_Last_Target = "hmmsii",
%%------------------------------------------------
										
	From = S1#electionState.currentRequest#user_request.from,
	To = S1#electionState.currentRequest#user_request.to,

	% Calcolo dei miei costi
	From = S1#electionState.currentRequest#user_request.from,
	To = S1#electionState.currentRequest#user_request.to,
	NearestColumnTarget = calculateNearestColumn(To),
	Dijkstra_Results = calculateDijkstra(Current_Last_Target, From, To, NearestColumnTarget),
	{CC, CRDT} = calculateSelfCost(Dijkstra_Results, Battery_level, Cost_To_Last_Target, S),
	%%------------------------------------------------

	S2 = S1#electionState { 
		flag_initiator = true,
		carsInvited = CloserCars,
		selfCost = {CC, CRDT}
	},

	if S2#electionState.carsInvited =:= [] -> 
		%print_debug_message(S2#electionState.pidCar, "No people to invite", []),
		{Cost_Client, Charge_After_Client} = S2#electionState.selfCost,
		Results = #electionCostData{
						pid_source = S2#electionState.pidCar, 
						cost_client = Cost_Client, 
						charge_after_transport = Charge_After_Client},
		%aspetto i risultati
		S3 = S2#electionState{totalCosts = [Results]},
		{next_state, initiator_final_state, S3};
	true ->  
		%print_debug_message(S2#electionState.pidCar, "All cars invited", []),
		%print_debug_message(S2#electionState.pidCar, "Waiting For: ~w", [S2#electionState.carsInvited]),
		
		%invio partecipateElection ai vicini
		DataPartecipate = #dataElectionPartecipate{
				pidParent = S2#electionState.pidCar,
				request = S2#electionState.currentRequest,
				ttl =  ?MAX_HOPES_ELECTION},
		S3 = S2#electionState{dataToSendPartecipate = DataPartecipate},
		{next_state, running_election, S3} 
	end;

idle(cast, {partecipateElection, Data}, S) ->
	%print_debug_message(S#electionState.pidCar, "Partecipate Election", []),
	% Update State
	S1 = S#electionState{currentRequest = Data#dataElectionPartecipate.request,
						parent = Data#dataElectionPartecipate.pidParent},

	Parent_Pid = S1#electionState.parent,
	sendMessageElection(Parent_Pid, {invite_result, {S1#electionState.pidCar, i_can_join}}, S1),

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

	CloserCars = lists:delete(S1#electionState.parent, CloserCars_All),
	%print_debug_message(S#electionState.pidCar, "Close Cars: ~w", [CloserCars]),

% Calcolo dei miei costi
	From = S1#electionState.currentRequest#user_request.from,
	To = S1#electionState.currentRequest#user_request.to,
	NearestColumnTarget = calculateNearestColumn(To),
	Dijkstra_Results = calculateDijkstra(Current_Last_Target, From, To, NearestColumnTarget),
	{CC, CRDT} = calculateSelfCost(Dijkstra_Results, Battery_level, Cost_To_Last_Target, S),
%%------------------------------------------------

	S2 = S1#electionState { 
		carsInvited = CloserCars,
		selfCost = {CC, CRDT}
	},

	%print_debug_message(S2#electionState.pidCar, "Current Parent Election: ~w", [S2#electionState.parent]),
	%print_debug_message(S2#electionState.pidCar, "Invited Cars: ~w", [S2#electionState.carsInvited]),
	if S2#electionState.carsInvited =:= [] -> 
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
			%invio partecipateElection ai vicini
			DataPartecipate = #dataElectionPartecipate{
					pidParent = S2#electionState.pidCar,
					request = S2#electionState.currentRequest,
					ttl = CurrentTTL - 1},
			S3 = S2#electionState{dataToSendPartecipate = DataPartecipate},
			{next_state, running_election, S3} 
	end.
	
%% ====================================================================
%% Running Election States functions
%% ====================================================================

running_election (enter, _OldState, S) ->
	%print_debug_message(S#electionState.pidCar, "Sending Invites to other cars", []),
	sendInvitesPartecipation(S#electionState.carsInvited, S#electionState.dataToSendPartecipate, S),
	keep_state_and_data;
	
running_election(info, {_From, tick}, _Stato) ->
	keep_state_and_data; %per ora non fare nulla
	
running_election(cast, {invite_result, Data}, S) -> 
	%print_debug_message(S#electionState.pidCar, "Running Election - Invite Results", []),
	{PID_Car, Partecipate} = Data,
	%print_debug_message(S#electionState.pidCar, "Car {~w} sended [~w]", [PID_Car, Partecipate]),
	NewInvited = lists:delete(PID_Car, S#electionState.carsInvited),
	OldChildrenPartecipate = S#electionState.childrenPartecipate,
	NewPartecipants =  
				if Partecipate == i_can_join 
						-> {_El, State} = searchPartecipantInList(OldChildrenPartecipate, PID_Car),
							if State == ok -> OldChildrenPartecipate;
									true -> 
										NewPartecipant = #carPartecipate{refCar = PID_Car, costsCar = [], sentResults = not_sended_results},
										[NewPartecipant] ++ OldChildrenPartecipate
							end;	
					true -> OldChildrenPartecipate
				end,
	S1 = S#electionState{carsInvited = NewInvited, 
				childrenPartecipate = NewPartecipants},

	FuncFilter = fun(Partecipant) -> Partecipant#carPartecipate.sentResults == not_sended_results end,
	MissingAnswers = lists:filter(FuncFilter, NewPartecipants),
	%print_debug_message(S1#electionState.pidCar, "Missing Answers: ~w", [MissingAnswers]),
	if  (MissingAnswers =:= []) and (NewInvited =:= []) -> 
			ChildrenCosts = packChildrenCosts(NewPartecipants),
			{Cost_Client, Charge_After_Client} = S1#electionState.selfCost,
			My_Results = #electionCostData{
							pid_source = S1#electionState.pidCar, 
							cost_client = Cost_Client, 
							charge_after_transport = Charge_After_Client},
			TotalCosts = [My_Results] ++ ChildrenCosts,
			if S1#electionState.flag_initiator -> 
					S2 = S1#electionState{totalCosts = TotalCosts},
					{next_state, initiator_final_state, S2};
				true -> 					
					%print_debug_message(S1#electionState.pidCar, "Sending Costs To Parent", []),
					Winner_Result = findBestResult(TotalCosts),
					sendMessageElection(S1#electionState.parent, {costs_results, {S1#electionState.pidCar, [Winner_Result]}}, S1),
				{next_state, waiting_final_results, S1} 
			end;
		true -> 
			{next_state, running_election, S1}	
	end;


running_election(cast, {costs_results, Data}, S) -> 
	%print_debug_message(S#electionState.pidCar, "Running Election - Costs Results", []),
	{PID_Sender , Results} = Data,
	%print_debug_message(S#electionState.pidCar, "Car {~w} sended [~w]", [PID_Sender, Results]),
	%print_debug_message(S#electionState.pidCar, "CurrentCarsInvited: ~w", [S#electionState.carsInvited]),
	%print_debug_message(S#electionState.pidCar, "CurrentPartecipants: ~w", [S#electionState.childrenPartecipate]),
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

	%print_debug_message(S1#electionState.pidCar, "NewCarsInvited: ~w", [S1#electionState.carsInvited]),
	%print_debug_message(S1#electionState.pidCar, "NewPartecipants: ~w", [S1#electionState.childrenPartecipate]),
	FuncFilter = fun(Partecipant) -> Partecipant#carPartecipate.sentResults == not_sended_results end,
	MissingAnswers = lists:filter(FuncFilter, NewPartecipants),
	%print_debug_message(S1#electionState.pidCar, "Missing Answers: ~w", [MissingAnswers]),
	if  (MissingAnswers =:= []) and (NewInvited =:= []) -> 
			ChildrenCosts = packChildrenCosts(NewPartecipants),
			{Cost_Client, Charge_After_Client} = S1#electionState.selfCost,
			My_Results = #electionCostData{
							pid_source = S1#electionState.pidCar, 
							cost_client = Cost_Client, 
							charge_after_transport = Charge_After_Client},
			TotalCosts = [My_Results] ++ ChildrenCosts,
			if S1#electionState.flag_initiator -> 
					S2 = S1#electionState{totalCosts = TotalCosts},
					{next_state, initiator_final_state, S2};
				true -> 					
					%print_debug_message(S1#electionState.pidCar, "Sending Costs To Parent", []),
                    Winner_Result = findBestResult(TotalCosts),
					sendMessageElection(S1#electionState.parent, {costs_results, {S1#electionState.pidCar, [Winner_Result]}}, S1),
				{next_state, waiting_final_results, S1} 
			end;
		true -> 
			{next_state, running_election, S1}	
	end.

%% ====================================================================
%% Final States functions
%% ====================================================================
initiator_final_state(enter, _OldState ,S) ->
	%print_debug_message(S#electionState.pidCar, "Calculating Final Results", []),
	TotalCosts = S#electionState.totalCosts,
	Winner_Partecipant = findBestResult(TotalCosts),
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
			print_debug_message(S#electionState.pidCar, "I Won Election", []);
		true ->
			ok
	end,
	FuncMap_2 = fun(Child) -> Child#carPartecipate.refCar end,
	Pids_To_notify = lists:map(FuncMap_2, S#electionState.childrenPartecipate),

	sendMessage(Pids_To_notify, {winning_results, Winner_Data}, S),
	sendToListener({election_results, [Winner_Data]}, S),
    keep_state_and_data;

initiator_final_state(cast, {exit_final_state_initator}, S) -> 
    S1 = resetState(S),
    {next_state, idle, S1}.

waiting_final_results (enter, _OldState, _State) ->
	keep_state_and_data;

waiting_final_results(info, {_From, tick}, _Stato) ->
	keep_state_and_data; %per ora non fare nulla

waiting_final_results(cast, {winning_results, Data}, S) -> 
	%print_debug_message(S#electionState.pidCar, "Waiting For Election Results", []),
	ID_Winner = Data#election_result_to_car.id_winner,
	My_Pid = S#electionState.pidCar,
	if
		My_Pid == ID_Winner ->
			_ID_APP_User = Data#election_result_to_car.id_app_user;
			% creo i record per la coda
			% aggiorno la coda movement all'automa
			%print_debug_message(S#electionState.pidCar, "I Won Election", []);
		true ->
			ok
	end,
	FuncMap_2 = fun(Child) -> Child#carPartecipate.refCar end,
	Pids_To_notify = lists:map(FuncMap_2, S#electionState.childrenPartecipate),
	sendMessage(Pids_To_notify, {winning_results, Data}, S),
	sendToListener({election_results, Data}, S),
	S1 = resetState(S),
	%print_debug_message(S1#electionState.pidCar, "Back to Idle", []),
	{next_state, idle, S1};

waiting_final_results(cast, {invite_result, _Data}, S) -> 
	{next_state, waiting_final_results, S}.

%% ====================================================================
%% Internal functions
%% ====================================================================
findBestResult(Results) ->
	SortFun = fun(A, B) -> 
		CostClient_A = A#electionCostData.cost_client, 
		Final_Charge_A = A#electionCostData.charge_after_transport,  
		CostClient_B = B#electionCostData.cost_client, 
		Final_Charge_B  = B#electionCostData.charge_after_transport, 
		if CostClient_A == CostClient_B -> Final_Charge_A > Final_Charge_B;
				true -> CostClient_A < CostClient_B
		end
	end,
	Sorted_Data = lists:sort(SortFun, Results),
% %print_debug_message(S#electionState.pidCar, "SortedData: ~w", [Sorted_Data]),
	Best_Partecipant = hd(Sorted_Data),
	Best_Partecipant.

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
	loopReceive().

loopReceive() ->
	receive
		{_From, tick} -> loopReceive();
		Data -> Data
	end.

%% ====================================================================
%% Cost functions
%% ====================================================================

calculateSelfCost(_Data, _Battery_level, _Cost_To_Last_Target, _S) -> 
	%print_debug_message(S#electionState.pidCar, "Battery Level: ~w", [Battery_level]),
	{utilities:generate_random_number(100), utilities:generate_random_number(100)}.

calculateDijkstra(_CurrentPosition, _From, _To, _NearestColumnTarget) -> [].


%% ====================================================================
%% Utilities functions
%% ====================================================================

calculateNearestColumn(_Node) -> "".

searchPartecipantInList(Partecipans, ToSearch) ->
	Out = lists:filter(fun(X) -> (X#carPartecipate.refCar == ToSearch) end, Partecipans),
	if Out =:= [] -> {[], none};
		true -> 
			El = hd(Out),
			{El, ok}
	end.

packChildrenCosts(List) -> packCosts(List, []).	

packCosts([], ACC) -> ACC;	
packCosts([H | T ], ACC) -> 
	NewACC = ACC ++ H#carPartecipate.costsCar,
	packCosts(T, NewACC).
