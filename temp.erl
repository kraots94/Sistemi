idle(cast, {partecipateElection, Data}, S) ->
    CurrentRequest =  Data#dataElectionPartecipate.request,
    Parent_Pid = Data#dataElectionPartecipate.pidParent,
	CurrentTTL = Data#dataElectionPartecipate.ttl,


    Self_Pid = S#electionState.pidCar
    sendMessageElection(Parent_Pid, {invite_result, {Self_Pid, i_can_join}}, S1),
    
	%%% Recupero dati da altri automi della macchina
	PID_GPS = S#electionState.pidGps,
	CloserCars_All = if CurrentTTL > 0 -> 
							getDataFromAutomata(PID_GPS, {self(), getNearCars}); 
						true -> 
							[] 
					end,

    CloserCars = lists:delete(Parent_Pid, CloserCars_All),
    %print_debug_message(S#electionState.pidCar, "Close Cars: ~w", [CloserCars]),

    SelfCost =  manage_self_cost(S, CurrentRequest),
    S1 = S#electionState{currentRequest = CurrentRequest,
        parent = Parent_Pid,
		carsInvited = CloserCars,
		selfCost = SelfCost
	},

	%print_debug_message(S2#electionState.pidCar, "Current Parent Election: ~w", [S2#electionState.parent]),
	%print_debug_message(S2#electionState.pidCar, "Invited Cars: ~w", [S2#electionState.carsInvited]),
	if CloserCars =:= [] -> 
			%mando i dati a mio padre
            ElectionCosts = create_election_cost_data(Self_Pid, SelfCost),
			sendMessageElection(Parent_Pid, {costs_results, {Self_Pid, [ElectionCosts]}}, S2),
			%aspetto i risultati
			{next_state, waiting_final_results, S2};
		true ->  
			%invio partecipateElection ai vicini
            DataPartecipate = create_data_partecipate(Self_Pid, CurrentRequest, CurrentTTL - 1),
			S3 = S2#electionState{dataToSendPartecipate = DataPartecipate},
			{next_state, running_election, S3} 
	end.