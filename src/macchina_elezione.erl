-module(macchina_elezione).
-import('send', [send_message/2, send_message/3]).
-import('utilities', [print_debug_message/1, 
						print_debug_message/2, 
						print_debug_message/3,		 
						print_car_message/1,
						print_car_message/2,
						print_car_message/3]).
-import('city_map', [get_nearest_col/3, calculate_path/2, create_records/5]).
-behaviour(gen_statem).
-export([start/5]).
-export([callback_mode/0, init/1, idle/3, running_election/3, 
		initiator_final_state/3, waiting_final_results/3]).
-include("records.hrl").
-include("globals.hrl").

-define(MAX_HOPS_ELECTION, 2).
-define(TIMEOUT_HOP, 500).

callback_mode() -> [state_functions, state_enter].

-record(carPartecipate, {refCar, costsCar, sentResults}).
-record(electionCostData, {pid_source, cost_client, charge_after_transport}).
%rappresentazione della richiesta dell'utente (usato da elezione)

-record(electionState, {
				pidCar,
				nameCar,
				pidMovingCar,
				pidGps,
				pidAppUser,
				parent,
				carsInvited,
				total_cars_invited,
				childrenPartecipate,
				total_children_partecipate,
				childrenCosts,
				cityMap,
				queueToManage,
				car_moving_queue_data,
				currentRequest,
				flag_initiator,
				my_election_cost,
				dataToSendPartecipate,
				totalCosts}).

%% ====================================================================
%% API functions
%% ====================================================================

start(PidMacchina, NomeMacchina, PidMovingCar, Pid_Gps_Car, City_Map) ->
	State = #electionState {
							pidAppUser = none,
							pidCar = PidMacchina,
							nameCar = NomeMacchina,
							pidMovingCar = PidMovingCar,
							pidGps = Pid_Gps_Car,
							parent = none,
							carsInvited = [],
							total_cars_invited = 0,
							childrenPartecipate = [],
							total_children_partecipate = 0,
							childrenCosts = [],
							cityMap = City_Map,
							queueToManage = [],
							car_moving_queue_data = none,
							currentRequest = {},
							flag_initiator = false,
							my_election_cost = [],
							dataToSendPartecipate = none,
							totalCosts = []},

	{ok, Pid} = gen_statem:start_link(?MODULE,State, []),
	Pid.

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

	print_debug_message(S#electionState.nameCar, "I have to begin Election For {~w}", Pid_App_User),

	{My_Election_Cost, S1} = manage_self_cost(S, CurrentRequest),

	S2 = S1#electionState { 
		flag_initiator = true,
		carsInvited = CloserCars,
		pidAppUser = Pid_App_User,
		currentRequest = CurrentRequest,
		my_election_cost = My_Election_Cost
	},

	if 
		CloserCars =:= [] -> 
			S3 = S2#electionState{totalCosts = My_Election_Cost},
			{next_state, initiator_final_state, S3};
		true ->  			
			DataPartecipate = create_data_partecipate(Self_Pid, CurrentRequest, ?MAX_HOPS_ELECTION),
			%invio partecipateElection ai vicini
			S3 = S2#electionState{dataToSendPartecipate = DataPartecipate},
			{next_state, running_election, S3, [{state_timeout,?TIMEOUT_HOP * (?MAX_HOPS_ELECTION + 1), timoutRunningElection}]} 
	end;

idle(cast, {partecipateElection, Data}, S) ->
	CurrentRequest = Data#dataElectionPartecipate.request,
	Parent_Pid = Data#dataElectionPartecipate.pidParent,
	CurrentTTL = Data#dataElectionPartecipate.ttl,

	Self_Pid = S#electionState.pidCar,
	sendMessageElection(Parent_Pid, {invite_result, {Self_Pid, i_can_join}}, S),
	
	%%% Recupero dati da altri automi della macchina
	PID_GPS = S#electionState.pidGps,
	CloserCars_All = if 
		CurrentTTL > 0 -> 
			getDataFromAutomata(PID_GPS, {self(), getNearCars}); 
		true -> 
			[] 
	end,

	CloserCars = lists:delete(Parent_Pid, CloserCars_All),

	{My_Election_Cost, S1} = manage_self_cost(S, CurrentRequest),
	S2 = S1#electionState{
		currentRequest = CurrentRequest,
		parent = Parent_Pid,
		carsInvited = CloserCars,
		my_election_cost = My_Election_Cost
	},

	if 
		CloserCars =:= [] -> 
			%mando i dati a mio padre
			sendMessageElection(Parent_Pid, {costs_results, {Self_Pid, My_Election_Cost}}, S2),
			%aspetto i risultati
			{next_state, waiting_final_results, S2};
		true ->  
			%invio partecipateElection ai vicini
			DataPartecipate = create_data_partecipate(Self_Pid, CurrentRequest, CurrentTTL - 1),
			S3 = S2#electionState{dataToSendPartecipate = DataPartecipate},
			{next_state, running_election, S3, [{state_timeout,?TIMEOUT_HOP * CurrentTTL, timoutRunningElection}]} 
	end;
	
idle({call,From}, {sendMovingQueue}, S) ->   
	QueueData = S#electionState.car_moving_queue_data,
	PidMovingCar = S#electionState.pidMovingCar,
	macchina_moving:updateQueue(PidMovingCar, QueueData, append),
	%resest stat
	S1 =  S#electionState{car_moving_queue_data = none},
	{keep_state, S1, [{reply,From,finished}]}.	

%% ====================================================================
%% Running Election States functions
%% ====================================================================

running_election (enter, _OldState, S) ->
	sendInvitesPartecipation(S#electionState.carsInvited, S#electionState.dataToSendPartecipate, S),
	keep_state_and_data;
	
running_election(info, {_From, tick}, _Stato) ->
	keep_state_and_data; %per ora non fare nulla

running_election(state_timeout, timoutRunningElection, Stato) -> 
	manage_end_running_election(Stato);

running_election(cast, {invite_result, Data}, S) -> 
	_Self_Pid = S#electionState.pidCar,
	{PID_Car, Partecipate} = Data,
	NewInvited = lists:delete(PID_Car, S#electionState.carsInvited),
	OldChildrenPartecipate = S#electionState.childrenPartecipate,
	NewPartecipants =  if
		Partecipate == i_can_join -> 
			{_El, Is_in_list} = searchPartecipantInList(OldChildrenPartecipate, PID_Car),
			if 
				Is_in_list == ok -> 
					OldChildrenPartecipate;
				true -> 
					NewPartecipant = #carPartecipate{refCar = PID_Car, 
													costsCar = [], 
													sentResults = not_sended_results},
					[NewPartecipant] ++ OldChildrenPartecipate
				end;	
		true -> 
			OldChildrenPartecipate
	end,
	S1 = S#electionState{carsInvited = NewInvited, 
				childrenPartecipate = NewPartecipants},
	
	NextState = calculate_next_state_running_election(S1),
	NextState;

running_election(cast, {costs_results, Data}, S) -> 
	{PID_Sender , Results} = Data,

	CurrentPartecipants = S#electionState.childrenPartecipate,
	CurrentCarsInvited = S#electionState.carsInvited,
	UserInInvitedList = lists:member(PID_Sender, CurrentCarsInvited),
	NewInvited = lists:delete(PID_Sender, CurrentCarsInvited),

	NewPartecipants = if
		UserInInvitedList ->
			NewPartecipant = #carPartecipate{
				refCar = PID_Sender,
				sentResults = ok, 
				costsCar = Results
			},
			[NewPartecipant] ++ CurrentPartecipants;
		true ->
			FuncMap = fun(Partecipant) -> 
				SentResults = Partecipant#carPartecipate.sentResults,
				NewPartecipant = if
					SentResults == ok -> 
						Partecipant;					
					true ->
						PID_Partecipant = Partecipant#carPartecipate.refCar,
						if 
							PID_Sender =:= PID_Partecipant -> 
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

	NextState = calculate_next_state_running_election(S1),
	NextState.

calculate_next_state_running_election(S) -> 
	InvitedCars = S#electionState.carsInvited,
	Partecipans = S#electionState.childrenPartecipate,
	FuncFilter = fun(Partecipant) -> Partecipant#carPartecipate.sentResults == not_sended_results end,
	MissingAnswers = lists:filter(FuncFilter, Partecipans),
	if  
		(MissingAnswers =:= []) and (InvitedCars =:= []) -> 
			manage_end_running_election(S);
		true -> 
			{next_state, running_election, S}	
	end.
%% ====================================================================
%% Final States functions
%% ====================================================================
initiator_final_state(enter, _OldState ,S) ->
	TotalCosts = S#electionState.totalCosts,
	Winner_Partecipant = findBestResult(TotalCosts),
	
	Winner_Data = if 
		Winner_Partecipant =:= [] -> 	
			#election_result_to_car{
				id_winner = -1,
				id_app_user = -1,
				request = {}
			};
		true -> 
			WinnerData = hd(Winner_Partecipant),
			Request = {S#electionState.currentRequest#user_request.from, 
						S#electionState.currentRequest#user_request.to},
			#election_result_to_car{
				id_winner = WinnerData#electionCostData.pid_source,
				id_app_user = S#electionState.pidAppUser,
				request = Request
			}
	end,
	S1 = manage_winner_data(Winner_Data, S),
%	timer:sleep(1000000),
	sendToListener({election_results, [Winner_Data]}, S1),
	{next_state, initiator_final_state, S1};

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
	S1 = manage_winner_data(Data, S),
	sendToListener({election_results, Data}, S1),	
	S2 = resetState(S1),
	{next_state, idle, S2};

waiting_final_results(cast, {invite_result, _Data}, S) -> 
	{next_state, waiting_final_results, S}.

%% ====================================================================
%% Internal functions
%% ====================================================================

resetState(S) ->
	S#electionState {
					pidAppUser = none,
					parent = none,
					carsInvited = [],
					total_cars_invited = 0,
					childrenPartecipate = [],
					total_children_partecipate = 0,
					childrenCosts = [],
					currentRequest = {},
					flag_initiator = false,
					my_election_cost = [],
					queueToManage = [],
					dataToSendPartecipate = none,
					totalCosts = []}.

%% If car can win returns his costs inside a list, otherwise empty list []
manage_self_cost(S, CurrentRequest) ->
	Self_Pid = S#electionState.pidCar,
	From = CurrentRequest#user_request.from,
	To = CurrentRequest#user_request.to,
	{Current_Cost_To_Last_Target, 
		Current_Last_Target, 
		Current_Battery_Level,
		CanPartecipate} = macchina_moving:getDataElection(S#electionState.pidMovingCar),
	if 
		CanPartecipate == ok ->
			City_Graph = S#electionState.cityMap#city.city_graph,
			City_Nodes = S#electionState.cityMap#city.nodes,
			City_Cols = S#electionState.cityMap#city.column_positions,
			NearestCol = get_nearest_col(To, City_Nodes, City_Cols),	
			Points = {Current_Last_Target, From, To, NearestCol},
			Battery_Avaiable = Current_Battery_Level - Current_Cost_To_Last_Target,
			CityData = {City_Graph, City_Nodes},
		
			{CC, CRDT, Can_Win, QueueCar} = calculateSelfCost(Points, Battery_Avaiable, CityData, S#electionState.nameCar),
			Final_CC = CC + Current_Cost_To_Last_Target,
			Out_Data = if 
				Can_Win == i_can_win  -> 
						ElectionCosts = create_election_cost_data(Self_Pid, {Final_CC, CRDT}),
						{[ElectionCosts], S#electionState{queueToManage = QueueCar}};
				true -> 
					   {[], S}
			end,
			Out_Data;
		true ->
			{[], S}
	end.

manage_end_running_election(S) ->
	Partecipans = S#electionState.childrenPartecipate,
	ChildrenCosts = packChildrenCosts(Partecipans),
	My_Election_Cost = S#electionState.my_election_cost,
	TotalCosts = My_Election_Cost ++ ChildrenCosts,
	if 
		S#electionState.flag_initiator -> 
			S1 = S#electionState{totalCosts = TotalCosts},
			{next_state, initiator_final_state, S1};
		true -> 					
			Winner_Result = findBestResult(TotalCosts),
			sendMessageElection(S#electionState.parent, {costs_results, {S#electionState.pidCar, Winner_Result}}, S),
			{next_state, waiting_final_results, S} 
	end.

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
	S1 = if
		ID_Winner == -1 -> 
			S;
		My_Pid == ID_Winner ->
			print_debug_message(S#electionState.nameCar, "I Won", none),
			ID_APP_User = Winner_Data#election_result_to_car.id_app_user,
			% creo i record per la coda partendo dalle queue già calcolate in fase begin / partecipate
			Queues = S#electionState.queueToManage,
			City_Nodes = S#electionState.cityMap#city.nodes,
			{Queue_P1_P2, Queue_P2_P3, Queue_P3_P4} = Queues,
			OutRecords = create_records(ID_APP_User, City_Nodes, Queue_P1_P2, Queue_P2_P3, Queue_P3_P4),
			% li salvo nel mio stato
			S#electionState{car_moving_queue_data = OutRecords};
		true ->
			S
	end,

	Childrens = S1#electionState.childrenPartecipate,
	if 
		Childrens =:= [] -> 
			ok;
		true ->
			%% Notify Children
			FuncMap = fun(Child) -> Child#carPartecipate.refCar end,
			Pids_To_notify = lists:map(FuncMap, Childrens),
			sendMessage(Pids_To_notify, {winning_results, Winner_Data}, S1)
	end,
	S1.

findBestResult(Results) ->
	Best_Partecipant = if 
		Results =:= [] -> 
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

calculateSelfCost(Points, Battery_Avaiable, CityData, Self_Name) -> 
	{P1, P2, P3, P4} = Points,
	{Cost_P1_P2, Queue_P1_P2} = calculate_path(CityData, {P1, P2}),
	RemainingCharge_P2 = Battery_Avaiable - Cost_P1_P2,
	Out_Results = if  
		RemainingCharge_P2 < 0 -> 	
			print_debug_message(Self_Name, "I have no battery for User Position", none),
			{-1, -1, i_can_not_win, []};		
		true -> 					
			{Cost_P2_P3, Queue_P2_P3} = calculate_path(CityData, {P2, P3}),
			RemainingCharge_P3 =  RemainingCharge_P2 - Cost_P2_P3,
			if  
				RemainingCharge_P3 < 0 -> 	
					print_debug_message(Self_Name, "I have no battery for Target Position", none),
					{-1, -1, i_can_not_win, []};
				true -> 					
					{Cost_P3_P4, Queue_P3_P4} = calculate_path(CityData, {P3, P4}),
					RemainingCharge_P4 = RemainingCharge_P3 -Cost_P3_P4,
					if  
						RemainingCharge_P4 < 0 -> 
							print_debug_message(Self_Name, "I have no battery for Column Position", none),
							{-1, -1, i_can_not_win, []};
						true ->	
							CC = Cost_P1_P2,
							CRDT = Battery_Avaiable - Cost_P1_P2 - Cost_P2_P3,
							QueueCar = {{Cost_P1_P2, Queue_P1_P2}, {Cost_P2_P3, Queue_P2_P3}, {Cost_P3_P4, Queue_P3_P4}},
							{CC, CRDT, i_can_win, QueueCar}
					end
			end
	end,
	Out_Results.

%% ====================================================================
%% Utilities functions
%% ====================================================================

searchPartecipantInList(Partecipans, ToSearch) ->
	Out = lists:filter(fun(X) -> (X#carPartecipate.refCar == ToSearch) end, Partecipans),
	if Out =:= [] -> 
			{[], none};
		true -> 
			El = hd(Out),
			{El, ok}
	end.

packChildrenCosts(List) -> packCosts(List, []).	

packCosts([], ACC) -> ACC;	
packCosts([H | T ], ACC) -> 
	NewACC = ACC ++ H#carPartecipate.costsCar,
	packCosts(T, NewACC).
