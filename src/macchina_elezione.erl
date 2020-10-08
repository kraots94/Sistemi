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
				total_cars_invited,
				childrenPartecipate,
				total_children_partecipate = 0,
				selfCost,
				childrenCosts,
				cityMap,
				tick_counter,
				currentRequest,
				flag_initiator,
				dijkstra_results,
				battery_level,
				total_tree_costs,
				my_election_cost,
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
							total_cars_invited = 0,
							childrenPartecipate = [],
							total_children_partecipate = 0,
							selfCost = {-1 , -1},
							childrenCosts = [],
							cityMap = City_Map,
						   	tick_counter = 0,
							currentRequest = {},
							flag_initiator = false,
							dijkstra_results = [],
							battery_level = 0,
							total_tree_costs = [],
							my_election_cost = [],
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
					total_cars_invited = 0,
					childrenPartecipate = [],
					total_children_partecipate = 0,
					selfCost = {-1 , -1},
					childrenCosts = [],
					tick_counter = 0,
					currentRequest = {},
					flag_initiator = false,
					dijkstra_results = [],
					battery_level = 0,
					total_tree_costs = [],
					my_election_cost = [],
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
	CurrentRequest =  Data#dataElectionBegin.request,
	Pid_App_User = Data#dataElectionBegin.pidAppUser,

	%%% Recupero dati da altri automi della macchina
	Self_Pid = S#electionState.pidCar,
	PID_GPS = S#electionState.pidGps,
	CloserCars = getDataFromAutomata(PID_GPS, {self(), getNearCars}),

	%print_debug_message(Self_Pid, "Close Cars: ~w", [CloserCars]),

	My_Election_Cost= manage_self_cost(S, CurrentRequest),

	S1 = S#electionState { 
		flag_initiator = true,
		carsInvited = CloserCars,
		pidAppUser = Pid_App_User,
		my_election_cost = My_Election_Cost
	},

	if CloserCars =:= [] -> 
			%print_debug_message(Self_Pid, "No people to invite", none),

			S2 = S1#electionState{totalCosts = My_Election_Cost},
			{next_state, initiator_final_state, S2};
		true ->  
			%print_debug_message(Self_Pid, "All cars invited", none),
			%print_debug_message(Self_Pid, "Waiting For: ~w", [CloserCars]),
			
			DataPartecipate = create_data_partecipate(Self_Pid, CurrentRequest, ?MAX_HOPES_ELECTION),
			%invio partecipateElection ai vicini
			S2 = S1#electionState{dataToSendPartecipate = DataPartecipate},
			{next_state, running_election, S2} 
	end;

idle(cast, {partecipateElection, Data}, S) ->
	CurrentRequest = Data#dataElectionPartecipate.request,
	Parent_Pid = Data#dataElectionPartecipate.pidParent,	CurrentTTL = Data#dataElectionPartecipate.ttl,

	Self_Pid = S#electionState.pidCar,
	sendMessageElection(Parent_Pid, {invite_result, {Self_Pid, i_can_join}}, S),
	
	%%% Recupero dati da altri automi della macchina
	PID_GPS = S#electionState.pidGps,
	CloserCars_All = if CurrentTTL > 0 -> 
							getDataFromAutomata(PID_GPS, {self(), getNearCars}); 
						true -> 
							[] 
					end,

	CloserCars = lists:delete(Parent_Pid, CloserCars_All),
	%print_debug_message(Self_Pid, "Close Cars: ~w", [CloserCars]),

	My_Election_Cost = manage_self_cost(S, CurrentRequest),
	S1 = S#electionState{currentRequest = CurrentRequest,
		parent = Parent_Pid,
		carsInvited = CloserCars,
		my_election_cost = My_Election_Cost
	},

	%print_debug_message(Self_Pid, "Current Parent Election: ~w", [Parent_Pid]),
	%print_debug_message(Self_Pid, "Invited Cars: ~w", [CloserCars]),
	if CloserCars =:= [] -> 
			%mando i dati a mio padre
			sendMessageElection(Parent_Pid, {costs_results, {Self_Pid, My_Election_Cost}}, S1),
			%aspetto i risultati
			{next_state, waiting_final_results, S1};
		true ->  
			%invio partecipateElection ai vicini
			DataPartecipate = create_data_partecipate(Self_Pid, CurrentRequest, CurrentTTL - 1),
			S2 = S1#electionState{dataToSendPartecipate = DataPartecipate},
			{next_state, running_election, S2} 
	end.
	
%% ====================================================================
%% Running Election States functions
%% ====================================================================

running_election (enter, _OldState, S) ->
	%print_debug_message(S#electionState.pidCar, "Sending Invites to other cars", none),
	sendInvitesPartecipation(S#electionState.carsInvited, S#electionState.dataToSendPartecipate, S),
	keep_state_and_data;
	
running_election(info, {_From, tick}, _Stato) ->
	keep_state_and_data; %per ora non fare nulla
	
running_election(cast, {invite_result, Data}, S) -> 
	_Self_Pid = S#electionState.pidCar,
	%print_debug_message(Self_Pid, "Running Election - Invite Results", none),
	{PID_Car, Partecipate} = Data,
	%print_debug_message(Self_Pid, "Car {~w} sended [~w]", [PID_Car, Partecipate]),
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
	
	NextState = calculate_next_state_running_election(S1),
	NextState;

running_election(cast, {costs_results, Data}, S) -> 
	_Self_Pid = S#electionState.pidCar,
	%print_debug_message(Self_Pid, "Running Election - Costs Results", none),
	{PID_Sender , Results} = Data,
	%print_debug_message(Self_Pid, "Car {~w} sended [~w]", [PID_Sender, Results]),
	CurrentPartecipants = S#electionState.childrenPartecipate,
	CurrentCarsInvited = S#electionState.carsInvited,
	UserInInvitedList = lists:member(PID_Sender, CurrentCarsInvited),
	NewInvited = lists:delete(PID_Sender, CurrentCarsInvited),
	%print_debug_message(Self_Pid, "CurrentCarsInvited: ~w", [CurrentCarsInvited]),
	%print_debug_message(Self_Pid, "CurrentPartecipants: ~w", [CurrentPartecipants]),
	NewPartecipants = if
		UserInInvitedList ->
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

	%print_debug_message(Self_Pid, "NewCarsInvited: ~w", [S1#electionState.carsInvited]),
	%print_debug_message(Self_Pid, "NewPartecipants: ~w", [S1#electionState.childrenPartecipate]),

	NextState = calculate_next_state_running_election(S1),
	NextState.

calculate_next_state_running_election(S) -> 
	InvitedCars = S#electionState.carsInvited,
	Partecipans = S#electionState.childrenPartecipate,
	FuncFilter = fun(Partecipant) -> Partecipant#carPartecipate.sentResults == not_sended_results end,
	MissingAnswers = lists:filter(FuncFilter, Partecipans),
	%print_debug_message(S#electionState.pidCar, "Missing Answers: ~w", [MissingAnswers]),
	if  (MissingAnswers =:= []) and (InvitedCars =:= []) -> 
			ChildrenCosts = packChildrenCosts(Partecipans),
			My_Election_Cost = S#electionState.my_election_cost,
			TotalCosts = My_Election_Cost ++ ChildrenCosts,

			if S#electionState.flag_initiator -> 
					S1 = S#electionState{totalCosts = TotalCosts},
					{next_state, initiator_final_state, S1};
				true -> 					
					%print_debug_message(S#electionState.pidCar, "Sending Costs To Parent", none),
					Winner_Result = findBestResult(TotalCosts),
					sendMessageElection(S#electionState.parent, {costs_results, {S#electionState.pidCar, Winner_Result}}, S),
					{next_state, waiting_final_results, S} 
			end;
		true -> 
			{next_state, running_election, S}	
	end.
%% ====================================================================
%% Final States functions
%% ====================================================================
initiator_final_state(enter, _OldState ,S) ->
	print_debug_message(S#electionState.pidCar, "Calculating Final Results", none),
	TotalCosts = S#electionState.totalCosts,
	Winner_Partecipant = findBestResult(TotalCosts),
	
	print_debug_message(S#electionState.pidCar, "Winner Data: ~w", Winner_Partecipant),
	Winner_Data = if 
			Winner_Partecipant =:= [] -> 	#election_result_to_car{
												id_winner = -1,
												id_app_user = -1
											};
			true -> 						
											WinnerData = hd(Winner_Partecipant),
											#election_result_to_car{
												id_winner = WinnerData#electionCostData.pid_source,
												id_app_user = S#electionState.pidAppUser
											}
				end,
	manage_winner_data(Winner_Data, S),
	sendToListener({election_results, [Winner_Data]}, S),
    keep_state_and_data;

initiator_final_state(info, {_From, tick}, _Stato) ->
	keep_state_and_data; %per ora non fare nulla

initiator_final_state(cast, {exit_final_state_initator}, S) -> 
    S1 = resetState(S),
    {next_state, idle, S1}.

waiting_final_results (enter, _OldState, _State) ->
	keep_state_and_data;

waiting_final_results(info, {_From, tick}, _Stato) ->
	keep_state_and_data; %per ora non fare nulla

waiting_final_results(cast, {winning_results, Data}, S) -> 
	%print_debug_message(S#electionState.pidCar, "Waiting For Election Results", none),
	manage_winner_data(Data, S),
	sendToListener({election_results, Data}, S),	
	S1 = resetState(S),
	%print_debug_message(S1#electionState.pidCar, "Back to Idle", none),
	{next_state, idle, S1};

waiting_final_results(cast, {invite_result, _Data}, S) -> 
	{next_state, waiting_final_results, S}.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% If car can win returns his costs inside a list, otherwise empty list []
manage_self_cost(S, CurrentRequest) ->
	Self_Pid = S#electionState.pidCar,
	From = CurrentRequest#user_request.from,
	To = CurrentRequest#user_request.to,
	%Current_Last_Target
	%Current_Cost_To_Last_Target
	%NearestColumnNode
	%Current_Battery_Level
	%City_Graph

	%{Cost_To_Last_Target, Current_Last_Target, Battery_Level} = getDataFromAutomata(PID_MOV, {getDataElection}),
	Battery_level = 200000,
	Cost_To_Last_Target = 0,
	Current_Last_Target = "hmmsii",
	%%------------------------------------------------	
	% Calcolo dei miei costi
	NearestColumnTarget = calculateNearestColumn(To),
	Dijkstra_Results = calculateDijkstra(Current_Last_Target, From, To, NearestColumnTarget),
	{CC, CRDT, Can_Win} = calculateSelfCost(Dijkstra_Results, Battery_level, Cost_To_Last_Target, S),
	%%------------------------------------------------
	ElectionCosts = create_election_cost_data(Self_Pid, {CC, CRDT}),
	My_Election_Cost = if Can_Win == i_can_win  -> [ElectionCosts];
								true -> []
				end,
	My_Election_Cost.

create_election_cost_data(Pid, {Cost_Client, Charge_After_Client}) ->
	Results = #electionCostData{
		pid_source = Pid, 
		cost_client = Cost_Client, 
		charge_after_transport = Charge_After_Client},
	Results.

create_data_partecipate(PidParent, Request, TTL) ->
	DataPartecipate = #dataElectionPartecipate{
		pidParent = PidParent,
		request = Request,
		ttl = TTL},
	DataPartecipate.

manage_winner_data(Winner_Data, S) ->
	ID_Winner = Winner_Data#election_result_to_car.id_winner,
	My_Pid = S#electionState.pidCar,
	if
		ID_Winner == -1 -> ok;
		My_Pid == ID_Winner ->
			_ID_APP_User = Winner_Data#election_result_to_car.id_app_user;
			% creo i record per la coda partendo dalle queue già calcolate in fase begin / partecipate

			% li salvo nel mio stato
			
			%print_debug_message(S#electionState.pidCar, "I Won Election", none);
		true ->
			ok
	end,

	Childrens = S#electionState.childrenPartecipate,
	if Childrens =:= [] -> ok;
			true ->
				%% Notify Children
				FuncMap = fun(Child) -> Child#carPartecipate.refCar end,
				Pids_To_notify = lists:map(FuncMap, Childrens),
				sendMessage(Pids_To_notify, {winning_results, Winner_Data}, S)
	end.

findBestResult(Results) ->
	Best_Partecipant = 
		if Results =:= [] -> 
				[];
			true ->
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
				[hd(Sorted_Data)]
			end,
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
	CC = utilities:generate_random_number(100) - 50,
	CRDT = utilities:generate_random_number(100) - 50,
	RandomNumber = utilities:generate_random_number(2),
	Can_Win = if RandomNumber == 1 -> i_can_win;
							true -> i_can_not_win
			end,
	Out = {CC, CRDT, Can_Win},
	Out.

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
