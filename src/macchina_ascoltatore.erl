%% @author Agnul
%% @doc @todo Add description to macchina_ascoltatore.

-module(macchina_ascoltatore).
-compile(export_all).
-behaviour(gen_statem).
-include("records.hrl").
-include("globals.hrl").
-import('utilities', [print_debug_message/1, 
						print_debug_message/2, 
						print_debug_message/3, 
						print_car_message/1,
						print_car_message/2,
						print_car_message/3, 
						generate_random_number/1]).
-import('city_map', [calculate_path/2, get_nearest_col/3, create_records/5]).

callback_mode() -> [state_functions].
-define(DEBUGPRINT_LISTENER, false).
-define(MAX_TIME_ELECTION, 3000).

-record(taxiListenerState, {pidMoving,
							pidBattery,
							pidElection,
							pidGps,
							pidClock,
							pidAppRequestingService,
							usersInQueue, %lista di record del tipo {Pid, Pos, Destination}
							userCarrying,
							name,
							city}). 

-record(userInQueue , {pid,
					   position,
					   target}).



%% ====================================================================
%% API functions
%% ====================================================================

%si crea con {nodo iniziale:stringa, PID_GPS_Server, mappa, nome:stringa}
start(InitData) ->
	{ok, Pid} = gen_statem:start_link(?MODULE,InitData, []),
	Pid.

beginElection(Pid) ->
	gen_statem:call(Pid, {beginElectionUser}).

updatePosition(ListenerPid, Position) ->
	gen_statem:cast(ListenerPid, {updatePosition, Position}).
	
changeDestination(ListenerPid, Request) ->
	gen_statem:call(ListenerPid, {changeDestination, Request}).

sendToEsternalAutomata(ListenerPid, Target, Data) ->
	gen_statem:cast(ListenerPid, {to_outside, {Target, Data}}).

crash(ListenerPid) ->
	gen_statem:cast(ListenerPid, {crash}).

fixCar(ListenerPid) ->
	gen_statem:cast(ListenerPid, {fixed}).

areYouKillable(Pid) ->
	gen_statem:call(Pid, {areYouKillable}).

dieSoft(ListenerPid) ->
	IsKillable = macchina_ascoltatore:areYouKillable(ListenerPid),
	if IsKillable ->
		   gen_statem:cast(ListenerPid, {die}),
		   killed;
	   true ->
		   not_killed
	end.

die(ListenerdPid) ->
	gen_statem:cast(ListenerdPid, {die}),
	killed.

%% ====================================================================
%% Automata functions
%% ====================================================================

init(InitData) -> 
	{InitialPos, PidGpsServer, City_Map, Name} = InitData,
	%creo pid delle entita' associate
	PidGpsModule = gps_module:start_gps_module(initDataGpsModule(PidGpsServer,InitialPos, Name)), %start gps module (and register)
	PidMoving = macchina_moving:start({InitialPos, self(), Name}),
	PidBattery  = macchina_batteria:start({PidMoving, Name}),
	PidElection = macchina_elezione:start(self(), Name, PidMoving,PidGpsModule,City_Map),
	PidClock = tick_server:start_clock([self(), PidMoving,PidBattery,PidElection]),
	State = #taxiListenerState {
					pidMoving   = PidMoving,
					pidBattery  = PidBattery,
					pidElection = PidElection,
					pidGps = PidGpsModule,
					pidClock = PidClock,
					pidAppRequestingService = -1,
					usersInQueue = [],
					userCarrying = "",
					name = Name,
					city = City_Map
			},
	print_car_message(State#taxiListenerState.name, "Car ready in position [~p]", InitialPos),
	{ok, idle, State}.

%ricezione del tick
idle(info, {_From, tick}, _Stato) -> keep_state_and_data; 

idle({call,From}, {areYouKillable}, State) ->
	PidMoving =  State#taxiListenerState.pidMoving,
	IsMovingKillable = macchina_moving:areYouKillable(PidMoving),
	if (IsMovingKillable) -> {keep_state, State, [{reply,From,true}]};
		true -> {keep_state, State, [{reply,From,false}]}
	end;

idle(cast, {die}, State) ->
	killEntities(State);

idle(cast, {crash}, State) ->
	print_car_message(State#taxiListenerState.name, "I am broken, now i notify my users and waiting for fix"),
	PidMoving = State#taxiListenerState.pidMoving,
	ListClients = State#taxiListenerState.usersInQueue,
	lists:foreach(fun(User) -> 
		AppUserPid = User#userInQueue.pid,
		gen_statem:cast(AppUserPid ,{crash})
		end, ListClients),
	gen_statem:cast(PidMoving, {crash}),
	{keep_state, State#taxiListenerState{usersInQueue = [], userCarrying = ""}};

idle(cast, {fixed}, State) ->
	print_car_message(State#taxiListenerState.name, "I am fixed, now I can serve again"),
	PidMoving = State#taxiListenerState.pidMoving,
	gen_statem:cast(PidMoving, {fixed}),
	keep_state_and_data;

idle(cast, {carrying, UserPid}, State) ->
	{keep_state, State#taxiListenerState{userCarrying = UserPid}};

idle(cast, {noMoreCarrying}, State) ->
	{keep_state, State#taxiListenerState{userCarrying = none}};

% data out of this car
idle(cast, {to_outside, {Target, Data}}, Stato) ->
	FinalTuple = if 
		is_tuple(Data) -> 
			{First, Second} = Data,
			if 
				First == newNodeReached ->
					ListUsersInQueue = Stato#taxiListenerState.usersInQueue,
					NewList = if 
						(Target == Stato#taxiListenerState.userCarrying) -> %è un cambio posizione del tipo che trasporto
							OldRecord = hd(ListUsersInQueue),
							if OldRecord#userInQueue.target == Second -> tl(ListUsersInQueue);
							   true ->
								   NewRecord = OldRecord#userInQueue{position = Second},
								   [NewRecord] ++ tl(ListUsersInQueue)
							end;
						true ->
							ListUsersInQueue
					end,
					{keep_state, Stato#taxiListenerState{usersInQueue = NewList}};
				true ->
					keep_state_and_data
			end;
		true -> 
			keep_state_and_data
	end,
	gen_statem:cast(Target, Data),
	FinalTuple;

%macchina vuole aggiornare posizione
idle(cast, {updatePosition, CurrentPosition}, Stato) ->
	PidGps = Stato#taxiListenerState.pidGps,
	gps_module:setPosition(PidGps, CurrentPosition),
	keep_state_and_data;
	
%begin election ricevuto da app utente
%Data = {From,To,PidAppUser}
idle(cast, {beginElection, Data}, Stato) ->
	print_debug_message(self(), "Have to start election, received request: ~w", Data),
	{From,To,PidAppUser} = Data,
	Request = #user_request{from = From, to = To},
	NewData = #dataElectionBegin{request = Request, pidAppUser = PidAppUser},
	PidElezione = Stato#taxiListenerState.pidElection,
	gen_statem:cast(PidElezione, {beginElection,NewData}),
	S1 = Stato#taxiListenerState{pidAppRequestingService = PidAppUser},
	{next_state, listen_election, S1, [{state_timeout, ?MAX_TIME_ELECTION, noReplyElectionInit}]};

%partecipate election ricevuto da altra macchina
idle(cast, {partecipateElection, Data}, Stato) ->
	print_debug_message(self(), "Request to partecipate election:  ~w", Data),
	PidElezione = Stato#taxiListenerState.pidElection,
	gen_statem:cast(PidElezione, {partecipateElection, Data}),
	{next_state, listen_election, Stato, [{state_timeout, ?MAX_TIME_ELECTION, noReplyElectionPartecipate}]};

idle({call, From}, {changeDestination, Request}, Stato) ->
	UsersInQueue = Stato#taxiListenerState.usersInQueue,
	if 
		length(UsersInQueue) == 1 -> %and implicitamente chi chiede è quello servito, altrimento non sarebbe arrivato qua
			Self_Name =  Stato#taxiListenerState.name,
			{Start, To, AppPid} = Request,
			PidMoving = Stato#taxiListenerState.pidMoving,
			BatteryLevel = macchina_moving:getBatteryLevel(PidMoving),
			CurrentPos = macchina_moving:getPosition(PidMoving),
			City_Map = Stato#taxiListenerState.city,
			Graph_City = City_Map#city.city_graph,
			City_Nodes = City_Map#city.nodes,
			City_Cols = City_Map#city.column_positions,			
			NearestCol = get_nearest_col(To, City_Nodes, City_Cols),	
			Points = {CurrentPos, Start, To, NearestCol},

			{_CC, _CRDT, Feasible, QueueCar} = calculateFeasible(Points, BatteryLevel, {Graph_City, City_Nodes}, Self_Name),
			if 
				Feasible == i_can_win ->
					{Queue_P1_P2, Queue_P2_P3, Queue_P3_P4} = QueueCar,
					OutRecords = create_records(AppPid, City_Nodes, Queue_P1_P2, Queue_P2_P3, Queue_P3_P4),
					macchina_moving:updateQueue(PidMoving, OutRecords, replace),
					OldListQueuedUsers = Stato#taxiListenerState.usersInQueue,
					OldRecord = hd(OldListQueuedUsers),
					NewRecord = OldRecord#userInQueue{target = To},
					NewListQueuedUsers = [NewRecord] ++ tl(OldListQueuedUsers),
					{keep_state, Stato#taxiListenerState{usersInQueue = NewListQueuedUsers}, [{reply, From, changed_path}]};
				true ->
					{keep_state, Stato, [{reply, From, not_enough_battery}]}
			end;
		true ->
			{keep_state, Stato, [{reply, From, user_in_car_queue}]}
	end;

idle(Whatever, What, Foo) ->
	keep_state_and_data.

%% ====================================================================
%% ELECTION functions
%% ====================================================================

listen_election(info, {_From, tick}, _Stato) ->
	keep_state_and_data; 

listen_election(state_timeout, noReplyElectionInit, Stato) ->
	PidElection = Stato#taxiListenerState.pidElection,
	gen_statem:stop(PidElection),
	Name = Stato#taxiListenerState.name,
	PidMoving = Stato#taxiListenerState.pidMoving,
	PidGpsModule = Stato#taxiListenerState.pidGps,
	City_Map = Stato#taxiListenerState.city,
	NewPidElection = macchina_elezione:start(self(), Name, PidMoving,PidGpsModule,City_Map),
	{next_state, idle, Stato#taxiListenerState{pidElection = NewPidElection}};

listen_election(state_timeout, noReplyElectionPartecipate, Stato) ->
	PidElection = Stato#taxiListenerState.pidElection,
	gen_statem:stop(PidElection),
	Name = Stato#taxiListenerState.name,
	PidMoving = Stato#taxiListenerState.pidMoving,
	PidGpsModule = Stato#taxiListenerState.pidGps,
	City_Map = Stato#taxiListenerState.city,
	NewPidElection = macchina_elezione:start(self(), Name, PidMoving,PidGpsModule,City_Map),
	{next_state, idle, Stato#taxiListenerState{pidElection = NewPidElection}};

%rimbalzo roba elezione a automa elettore
listen_election(cast, {election_data, Data}, Stato) ->	
	%print_debug_message(self(), "Listener - election_data - ~w", [Data]),
	PidElezione = Stato#taxiListenerState.pidElection,
	gen_statem:cast(PidElezione, Data),
	keep_state_and_data;

% data out of this car
listen_election(cast, {to_outside, {Target, Data}}, Stato) ->
	FinalTuple = if 
		is_tuple(Data) -> 
			{First, Second} = Data,
			if 
				First == newNodeReached ->
					ListUsersInQueue = Stato#taxiListenerState.usersInQueue,
					NewList = if 
						(Target == Stato#taxiListenerState.userCarrying) -> %è un cambio posizione del tipo che trasporto
							OldRecord = hd(ListUsersInQueue),
							if OldRecord#userInQueue.target == Second -> tl(ListUsersInQueue);
							   true ->
								   NewRecord = OldRecord#userInQueue{position = Second},
								   [NewRecord] ++ tl(ListUsersInQueue)
							end;
						true ->
							ListUsersInQueue
					end,
					{keep_state, Stato#taxiListenerState{usersInQueue = NewList}};
				true ->
					keep_state_and_data
			end;
		true -> 
			keep_state_and_data
	end,
	gen_statem:cast(Target, Data),
	FinalTuple;

listen_election(cast, {beginElection, Data}, _Stato) ->
	{_From, _To, PidAppUser} = Data,
	gen_statem:cast(PidAppUser, {already_running_election_wait}),
	keep_state_and_data;

listen_election(cast, {partecipateElection, Data}, _Stato) ->
	PidSender = Data#dataElectionPartecipate.pidParent,
	gen_statem:cast(PidSender, {election_data, {invite_result, {self(), i_can_not_join}}}),
	keep_state_and_data;

listen_election(cast, {election_results, Data}, Stato) ->
	PidElezione = Stato#taxiListenerState.pidElection,
	{DataToUse, Is_Initiator} = if 
		is_list(Data) -> %meaning received by initiator
			% Tolgo l'elezione dallo stato di calcolo
			gen_statem:cast(PidElezione, {exit_final_state_initator}),
			{hd(Data), true};
		true -> 
			{Data, false}
	end,

	Pid_Car = DataToUse#election_result_to_car.id_winner,
	NewState = if 
		(Pid_Car == -1) and (Is_Initiator) -> % Avviso l'utente se non c'è un vincitore
			notify_user_no_taxi(Stato#taxiListenerState.pidAppRequestingService),
			Stato;
		Pid_Car == self() -> 
			print_debug_message(self(), "I have won election"),
			gen_statem:call(PidElezione, {sendMovingQueue}), %update queue moving
			PidMoving = Stato#taxiListenerState.pidMoving,
			Pid_User = DataToUse#election_result_to_car.id_app_user,
			TimeToUser = macchina_moving:getTimeToUser(PidMoving),
			Name = Stato#taxiListenerState.name,
			DataToUser = #election_result_to_user{
				id_car_winner = Pid_Car, 
				name_car = Name,
				time_to_wait = TimeToUser
			},
			NewListQueuedUsers = calculateNewListQueuedUsers(Stato, DataToUse),
			gen_statem:cast(Pid_User, {winner, DataToUser}),
			Stato#taxiListenerState{usersInQueue = NewListQueuedUsers};
		true -> Stato
	end,
	{next_state, idle, NewState};

%tutti altri eventi posticipali
listen_election(_Whatever, OtherEvents, Stato) ->
	{keep_state, Stato, [postpone]}.

%% ====================================================================
%% Internal functions
%% ====================================================================

initDataGpsModule(PidGpsServer,InitialPosition, Name) ->
	#dataInitGPSModule{
		pid_entity = self(), 
		name_entity = Name,
		type = car, 
		pid_server_gps = PidGpsServer,
		starting_pos = InitialPosition, 
		signal_power = ?GPS_MODULE_POWER, 
		map_side = ?MAP_SIDE}.

notify_user_no_taxi(PID) ->
	Pid_User = PID,
	DataToUser = #election_result_to_user{
		id_car_winner = -1, 
		time_to_wait = -1
	},
	gen_statem:cast(Pid_User, {winner, DataToUser}).

killEntities(State) ->
	PidMov = State#taxiListenerState.pidMoving,
	PidBattery =  State#taxiListenerState.pidBattery,
	PidElection = State#taxiListenerState.pidElection,
	PidGps = State#taxiListenerState.pidGps,
	PidClock = State#taxiListenerState.pidClock,
	gen_statem:stop(PidMov),gen_statem:stop(PidBattery),gen_statem:stop(PidElection), %ammazzo automi
	gps_module:end_gps_module(PidGps), tick_server:end_clock(PidClock), %ammazzo server
	print_debug_message(self(), "All linked entities killed"),
	gen_statem:stop(self()).

calculateNewListQueuedUsers(State, ElectionData) ->
	PidNewUser = ElectionData#election_result_to_car.id_app_user,
	Request =  ElectionData#election_result_to_car.request,
	{From, To} = Request,
	OldList = State#taxiListenerState.usersInQueue,
	NewRecord = #userInQueue{pid = PidNewUser,
							 position = From,
						     target = To},
	OldList ++ [NewRecord].

printDebug(ToPrint) ->
	if ?DEBUGPRINT_LISTENER -> print_debug_message(self(), [?TILDE_CHAR] ++ "p", ToPrint);
	   true -> not_printed
	end.


calculateFeasible(Points, Battery_Avaiable, CityData, Self_Name) -> 
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