idle(cast, {beginElection, Data}, S) ->   
CurrentRequest =  Data#dataElectionBegin.request,
Pid_App_User = Data#dataElectionBegin.pidAppUser,

%%% Recupero dati da altri automi della macchina
	PID_GPS = S#electionState.pidGps,
	CloserCars = getDataFromAutomata(PID_GPS, {self(), getNearCars}),

	%print_debug_message(S#electionState.pidCar, "Close Cars: ~w", [CloserCars]),
	

    SelfCost =  manage_self_cost(S, CurrentRequest)
	S1 = S#electionState { 
		flag_initiator = true,
		carsInvited = CloserCars,
        selfCost = SelfCost,
        pidAppUser = Pid_App_User
	},

	if S2#electionState.carsInvited =:= [] -> 
		%print_debug_message(S2#electionState.pidCar, "No people to invite", []),
        ElectionCosts = create_election_cost_data(Self_Pid, SelfCost),
		S2 = S1#electionState{totalCosts = [ElectionCosts]},
		{next_state, initiator_final_state, S2};
	true ->  
		%print_debug_message(S2#electionState.pidCar, "All cars invited", []),
		%print_debug_message(S2#electionState.pidCar, "Waiting For: ~w", [S2#electionState.carsInvited]),
		
        DataPartecipate = create_data_partecipate(Self_Pid, CurrentRequest, ?MAX_HOPES_ELECTION),
		%invio partecipateElection ai vicini
		S3 = S1#electionState{dataToSendPartecipate = DataPartecipate},
		{next_state, running_election, S3} 
	end;