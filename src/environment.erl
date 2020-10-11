-module(environment).

-import('tick_server',[start_clock/1, end_clock/1]).
-import('gps_server', [start_gps_server/1, end_gps_server/1]).
-import('send', [send_message/2, send_message/3]).
-import('city_map', [init_city/0]).
-import('nodes_util',[getRandomPositionName/2]).
-import('utilities', [print_debug_message/1, 
						print_debug_message/2, 
						print_debug_message/3,
						print_environment_message/1,
						print_environment_message/2,
						print_environment_message/3,
						getRandomEntity/2,
						construct_string/2,
						getDictionaryKeys/1]).

-include("records.hrl").
-include("globals.hrl").
-define(MAX_KILL_TRIES, 20).

% State environment:

-record(environmentState,  {cars,
							total_cars,
							users, 
							total_users,
							cars_crashed,
							total_cars_crashed,
							city, 
							tick_s_pid, 
							pid_gps_server, 
							tick_counter,
							autoEvents,
							user_pids,
							last_user_id,
							user_prefix = u,
							car_pids,
							last_car_id,
							car_prefix = c}).
						
%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, end_environment/1, triggerEvent/2,
		enableAutoEvents/1, disableAutoEvents/1, printGpsState/1, 
		printSelfState/1, spawnCars/2, spawnUsers/2, removeCar/2, removeUser/2,
		triggerChangeDestUser/2, triggerChangeDestUser/3, triggerCarCrash/2, 
		triggerFixCar/2, triggerCarRemove/2, printCars/1, printUsers/1]).

start_link() -> spawn_link(fun init/0).

triggerEvent(PID_ENV, ID) ->
	send_message(PID_ENV, {event, ID}).

triggerChangeDestUser(PID_ENV, UserName) -> 
	triggerChangeDestUser(PID_ENV, UserName, none).

triggerChangeDestUser(PID_ENV, UserName, NewTarget) ->
	send_message(PID_ENV, {event, 3, UserName, NewTarget}).

triggerCarCrash(PID_ENV, CarName) -> 
	send_message(PID_ENV, {event, 4, CarName}).

triggerFixCar(PID_ENV, CarName) -> 
	send_message(PID_ENV, {event, 5, CarName}).

triggerCarRemove(PID_ENV, CarName) -> 
	send_message(PID_ENV, {event, 10, CarName}).

enableAutoEvents(PID_ENV) ->
	send_message(PID_ENV, {enable_auto_events}).

disableAutoEvents(PID_ENV) ->
	send_message(PID_ENV, {disable_auto_events}).

printGpsState(PID_ENV) ->
	send_message(PID_ENV, {print, gps_server_state}).

printSelfState(PID_ENV) ->
	send_message(PID_ENV, {print, self}).

printCars(PID_ENV) ->
	send_message(PID_ENV, {print, self, cars}).

printUsers(PID_ENV) ->
	send_message(PID_ENV, {print, self, users}).

spawnCars(PID_ENV, N) ->
	send_message(PID_ENV, {spawn, cars, N}).

spawnUsers(PID_ENV, N) ->
	send_message(PID_ENV, {spawn, users, N}).

spawnCar(PID_ENV, StartingPos) ->
	send_message(PID_ENV, {spawn, car, StartingPos}).

spawnUser(PID_ENV, From, To) ->
	send_message(PID_ENV, {spawn, user, From, To}).

% Just removes ref from internal, does not kill
removeCar(PID_ENV, Pid) ->
	send_message(PID_ENV, {task_complete, car, Pid}).

% Just removes ref from internal, does not kill	
removeUser(PID_ENV, Pid) -> 
	send_message(PID_ENV, {task_complete, user, Pid}).

end_environment(Pid) ->
	print_debug_message("Killing Environment"),
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
%% Server functions
%% ====================================================================

init() -> 
	print_debug_message("Start Environment"),
	City = init_city(),
	Pid_Tick = start_clock([self()]),
	PID_server_gps = start_gps_server(City#city.nodes),
	S = #environmentState{cars = [], 
						  users = [], 
						  cars_crashed = [],
						  total_cars = 0,
						  total_users = 0,
						  total_cars_crashed = 0,
						  city = City, 
						  tick_s_pid = Pid_Tick, 
						  pid_gps_server = PID_server_gps, 
						  tick_counter = 0,
						  autoEvents = false,
						  user_pids = dict:new(),
						  last_user_id = 1,
						  car_pids = dict:new(),
						  last_car_id = 1},
	print_environment_message(self(), "Environment Created"),
	loop(S).

loop(State) ->
    receive
		{_Pid, tick} -> 
			NewState = if 
				State#environmentState.autoEvents ->
					CurrentTickCounter = State#environmentState.tick_counter,
					NextTick = CurrentTickCounter + 1,
					if NextTick >= ?TICKS_EVENT -> 
							HandledState = handle_random_event(State, utilities:generate_random_number(100)),
							HandledState#environmentState{tick_counter = 0};
					true -> 
						State#environmentState{tick_counter = NextTick}
					end;
				true -> 
					State
			end,
			loop(NewState);
		
		{event, EventID} -> 
			loop(handle_event(State, EventID, {none}));

		{event, 3, UserName, NewTarget} ->  % Change target
			Out = getPidFromDict(UserName, State#environmentState.user_pids),
			NewState = if
				Out == error -> 
					print_environment_message(self(), "User with Name [~p] does not exists", [UserName]),
					State;
				true -> 
					{ok, Pid} = Out,
					handle_event(State, 3, {Pid, NewTarget})
			end,
			loop(NewState);		

		{event, 4, Target} -> % Car crash
			Out = getPidFromDict(Target, State#environmentState.car_pids),
			NewState = if
				Out == error -> 
					print_environment_message(self(), "Car with Name [~p] does not exists", [Target]),
					State;
				true -> 
					{ok, Target} = Out,
					handle_event(State, 4, {Target})
			end,
			loop(NewState);				
		
		{event, 5, Target} -> % Fix car
			Out = getPidFromDict(Target, State#environmentState.car_pids),
			NewState = if
				Out == error -> 
					print_environment_message(self(), "Car with Name [~p] does not exists", [Target]),
					State;
				true -> 
					{ok, Target} = Out,
					handle_event(State,5, {Target})
			end,
			loop(NewState);		

		{event, 10, Target} -> % Remove car
			Out = getPidFromDict(Target, State#environmentState.car_pids),
			NewState = if
				Out == error -> 
					print_environment_message(self(), "Car with Name [~p] does not exists", [Target]),
					State;
				true -> 
					{ok, Target} = Out,
					handle_event(State, 10, {Target})
			end,
			loop(NewState);	

		{enable_auto_events} ->
			print_environment_message(self(), "Auto Events Enabled"),
			loop(State#environmentState{autoEvents = true});
		{disable_auto_events} ->
			print_environment_message(self(), "Auto Events Disabled"),
			loop(State#environmentState{autoEvents = false});
		{print, gps_server_state} -> 
			State#environmentState.pid_gps_server ! {self(), {printState}},
			loop(State);
		{print, self} -> 
			print_environment_message(self(), "Environment State: ~w", State),
			loop(State);
		{print, self, cars} ->
			print_environment_message(self(), "Cars in city: ~p", getDictionaryKeys(State#environmentState.car_pids)),
			loop(State);
		{print, self, users} ->
			print_environment_message(self(), "Users in city: ~p", getDictionaryKeys(State#environmentState.user_pids)),
			loop(State);
		{spawn, car, StartingPos} -> 
			Nodes = State#environmentState.city#city.nodes,
			StartingPosID = nodes_util:getID(StartingPos, Nodes),
			NewState = if 
				StartingPosID == -1 ->
					print_environment_message(self(), "Node with name {~p} does not exist", StartingPos),
					State;
				true ->
					generate_single_car(State, StartingPos)
			end,
			loop(NewState);
		{spawn, user, From, To} ->
			loop(generate_single_car(State, {From, To}));
		{spawn, cars, N} ->
			print_debug_message(self(), "Have to spawn ~w cars", N),
			NewState = generate_multiple_cars(State, N),
			loop(NewState);
		{spawn, users, N} -> 
			print_debug_message(self(), "Have to spawn ~w users", N),
			NewState = generate_multiple_users(State, N),
			loop(NewState);
		{task_complete, car, Pid} -> 	
			print_debug_message(self(), "Have to remove car with Pid {~w}", Pid),
			NewState = removeCarFromEnvironment(Pid, State),
			loop(NewState);
		{task_complete, user, Pid} -> 	
			print_debug_message(self(), "Have to remove user with Pid {~w}", Pid),
			NewState = removeUserFromEnvironment(Pid, State),
			loop(NewState);
        {Pid, Ref, terminate} ->
			send_message(Pid, {Ref, ok}),
			terminate(State),
			print_debug_message(self(), "Exiting enviroment loop");
        Unknown ->
			print_debug_message(self(), "Environment Received Unknown: ~p", Unknown),
            loop(State)
    end.

%% ====================================================================
%% Handle Events functions
%% ====================================================================

handle_random_event(S, N) ->
	EventID = if 	
	    N < 0   ->  -1;
		N < 10  ->   1;
	   	N < 15  ->  -1;
		N < 25  ->   2;
		N < 35  ->  -1;
		N < 45  ->   3;
		N < 50  ->  -1;
		N < 60  ->   4;
		N < 70  ->   5;
		N < 75  ->  -1;
		N < 77  ->   6;
		N == 77 ->  -1;
		N < 80  ->   7;
		N == 80 ->  -1;
		N < 83  ->   8;
		N == 83 ->  -1;
		N < 86  ->   9;
		N < 95  ->  -1;
		N < 100 ->  10;
		true    ->   -1
	end,
	handle_event(S, EventID, {none}).

handle_event(S, EventID, Data) ->
	NewState = if
		EventID == -1 -> % Nothing
			print_debug_message(self(), "Event: ~p", "nothing happened"),
			S;
		EventID == 1 -> % Spawn car
			print_debug_message(self(), "Event: ~p", "spawn car"),
			event_generate_car(S);
		EventID == 2 -> % Spawn user
			print_debug_message(self(), "Event: ~p", "spawn user"),
			event_generate_user(S);
		EventID == 3 -> % Change target
			print_debug_message(self(), "Event: ~p", "client change target"),
			event_user_change_target(S, Data);
		EventID == 4 -> % Car crash
			print_debug_message(self(), "Event: ~p", "car crash"),
			event_car_crash(S, Data);
		EventID == 5 -> % Fix car
			print_debug_message(self(), "Event: ~p", "fix car"),
			event_fix_car(S, Data);
		EventID == 6 -> % Add node to map
			print_debug_message(self(), "Event: ~p", "add node to map"),
			S;
		EventID == 7 -> % Add street to map
			print_debug_message(self(), "Event: ~p", "add street to map"),
			S;
		EventID == 8 -> % Remove node from map
			print_debug_message(self(), "Event: ~p", "remove node from map"),
			S;
		EventID == 9 -> % Remove street from map
			print_debug_message(self(), "Event: ~p", "remove street from map"),
			S;
		EventID == 10 -> % Remove car
			print_debug_message(self(), "Event: ~p", "remove car from environment"),
			event_remove_car(S, Data);
		true ->
			print_debug_message(self(), "Event ~w does not exist", EventID),
			S
	end,
	NewState.

%% ====================================================================
%% Event functions
%% ====================================================================

event_generate_car(S) ->
	generate_single_car(S, none).

event_generate_user(S) ->
	generate_single_user(S, none).

event_user_change_target(S, Data) ->
	ListData = tuple_to_list(Data),
	TargetTest = hd(ListData),
	{TargetUser, NewDest} = if 
		TargetTest == none -> 
			{getRandomUser(S), getRandomNode(S)}; 
		true -> 
			{User, PassedDest} = Data,
			if 
				PassedDest == none -> 
					{User, getRandomNode(S)};
				true -> 
					{User, PassedDest}
			end
	end,

 	UserChangedDest = changeDestUser(TargetUser, NewDest),
	if 
		UserChangedDest == changed ->
			print_environment_message(self(), "User {~w} has now new target [~p]", [TargetUser, NewDest]);
		true ->
			print_environment_message(self(), "User {~w} has not changed his destination to [~p]", [TargetUser, NewDest])
	end,
	S.

event_car_crash(S, Data) ->
	{Target} = Data,
	Total_Cars = S#environmentState.total_cars,
	Total_Crashed = S#environmentState.total_cars_crashed,
	if
		Total_Cars == Total_Crashed ->
			print_environment_message(self(), "Event car crash not occurred because all cars are already crashed"),
			S;
		true ->					
			CrashedCars = S#environmentState.cars_crashed,
			Car_To_Crash = if
				Target == none -> 
					searchCarNotCrashed(S, CrashedCars, true, -1);
				true -> 
					Is_Member = lists:member(Target, CrashedCars),
					if Is_Member ->
							already_crashed;
						true ->
							Target
					end
			end,
			if 	
				Car_To_Crash == already_crashed -> 
					print_environment_message(self(), "Car already out of games!"),
					S;
				true ->
					New_crashed_cars = [Car_To_Crash] ++ CrashedCars,
					New_total_cars_crashed = Total_Crashed + 1,
					%TODO Write here code to notify car

					print_environment_message(self(), "Car with pid {~w} has punctured rubber of the car!", Car_To_Crash),
					S#environmentState{
						cars_crashed = New_crashed_cars,
						total_cars_crashed = New_total_cars_crashed
					}
			end
	end.

event_fix_car(S, Target) ->
	Total_Crashed = S#environmentState.total_cars_crashed,
	if
		Total_Crashed == 0 ->
			print_debug_message(self(), "Event car fix not occurred because no cars are crashed"),
			S;
		true ->
			CrashedCars = S#environmentState.cars_crashed,
			Car_To_Fix = if
				Target == none -> 
					getRandomCrashedCar(S);
				true -> 
					Is_Member = lists:member(Target, CrashedCars),
					if Is_Member ->
							Target;
						true ->
							already_fixed
					end
			end,
			if 
				Car_To_Fix == already_fixed ->
					print_environment_message(self(), "Car is already fixed!"),
					S;
				true ->
					New_crashed_cars = lists:delete(Car_To_Fix, CrashedCars),
					New_total_cars_crashed = Total_Crashed - 1,
					%TODO Write here code to notify car
				
					print_environment_message(self(), "Car with pid {~w} has been fixed!", Car_To_Fix),
					S#environmentState{
						cars_crashed = New_crashed_cars,
						total_cars_crashed = New_total_cars_crashed
					}
			end
	end.

event_remove_car(S, Target) ->
	Total_Cars = S#environmentState.total_cars,
	if 
		Total_Cars == 0 ->
			print_debug_message(self(), "Event kill car not occurred because no cars in environment"),
			S;
		true ->
			Cars = S#environmentState.cars,
			NewState = if
				Target == none -> 
					Pid_Removed = findAndKillCar(S, not_killed, -1, 0),
					if 	
						Pid_Removed /= -1 ->
							removeCarFromEnvironment(Pid_Removed, S),
							print_environment_message(self(), "Car with pid {~w} has been removed!", Pid_Removed);
						true ->
							print_debug_message(self(), "Event kill car not occurred because not found car in idle"),
							S
					end;
				true -> 
					Is_Member = lists:member(Target, Cars),
					if 
						Is_Member ->
							Out = kill_car(Target, false),
							if 	Out == killed ->
								removeCarFromEnvironment(Target, S),
								print_environment_message(self(), "Car with pid {~w} has been removed!", Target);
							true ->
								print_environment_message(self(), "Event kill car not occurred because car was not in idle"),
								S
						end;
					true ->
						print_environment_message(self(), "Car has been already been removed"),
						S
					end
			end,
			NewState
	end.
	
%% ====================================================================
%% Internal functions
%% ====================================================================

generate_multiple_cars(State, N) ->
	{NewState, Cars_Generated} = generate_entities(State, none, N, fun generate_taxi/2, fun updateLastCarName/1, []),
	{Pids_Dict, Pids_List} = updateRefs(Cars_Generated, NewState#environmentState.car_pids, NewState#environmentState.cars),
	Total_Cars = NewState#environmentState.total_cars + N,
	NewState#environmentState{	cars = Pids_List, 
								total_cars = Total_Cars,
								car_pids = Pids_Dict}.

generate_multiple_users(State, N) -> 
	{NewState, Users_Generated} = generate_entities(State, none,  N, fun generate_user/2, fun updateLastUserName/1, []),
	{Pids_Dict, Pids_List} = updateRefs(Users_Generated, NewState#environmentState.user_pids, NewState#environmentState.users),
	Total_Users = NewState#environmentState.total_users + N,
	NewState#environmentState{	users = Pids_List, 
								total_users = Total_Users,
								user_pids = Pids_Dict}.
	
generate_single_car(State, Data) ->
	{NewState, TaxiData} =  generate_entities(State, Data, 1, fun generate_taxi/2, fun updateLastCarName/1, []),
	{Pids_Dict, Pids_List} = updateRefs(TaxiData, NewState#environmentState.car_pids, NewState#environmentState.cars),
	Total_Cars = State#environmentState.total_cars + 1,
	NewState#environmentState{	cars = Pids_List, 
								total_cars = Total_Cars,
								car_pids = Pids_Dict}.

generate_single_user(State, Data) ->
	{NewState, UserData} = generate_entities(State, Data, 1, fun generate_user/2, fun updateLastUserName/1, []),
	{Pids_Dict, Pids_List} = updateRefs(UserData, NewState#environmentState.user_pids, NewState#environmentState.users),
	Total_Users = NewState#environmentState.total_users + 1,
	NewState#environmentState{	users = Pids_List, 
								total_users = Total_Users,
								user_pids = Pids_Dict}.

generate_taxi(State, Pos) ->
	StartingPos = if 
		Pos == none -> 
			getRandomNode(State);
		true -> 
			Pos
	end,
	PidGpsServer = State#environmentState.pid_gps_server,
	City_Map = State#environmentState.city,
	TaxiName = getUpdatedTaxiName(State),
	PidTaxi = macchina_ascoltatore:start({StartingPos, PidGpsServer, City_Map, TaxiName}),
	{TaxiName, PidTaxi}.

generate_user(State, Request) ->
	GeneratedRequest = if
		Request == none ->
			NodeStart = getRandomNode(State),
			generateUserRequest(State, NodeStart);
		true -> Request
	end,
	{From, _To} = GeneratedRequest,
	PidGpsServer = State#environmentState.pid_gps_server,
	UserName = getUpdatedUserName(State),
	PidUtente = utente:start({From, PidGpsServer, UserName, self()}),
	utente:sendRequest(PidUtente, Request),
	{UserName, PidUtente}.

kill_car(Pid, ForceKill) ->
	Out = if 
		ForceKill -> 
			macchina_ascoltatore:die(Pid);
		true ->
			macchina_ascoltatore:dieSoft(Pid)
	end,
	if 
		Out == killed ->
			print_debug_message(self(), "Car with PID {~w} has been removed", Pid);
		true ->
			print_debug_message(self(), "Car with PID {~w} has been not removed", Pid)
	end,
	Out.

removeCarFromEnvironment(Pid, S) ->
	Cars = S#environmentState.cars,
	Dict = S#environmentState.car_pids,
	Total_Cars = S#environmentState.total_cars,
	NewCars = lists:delete(Pid, Cars),
	New_Total_Cars = Total_Cars - 1,
	FilterFunc = fun(_Key, Val) -> hd(Val) /= Pid end,
	NewDict = dict:filter(FilterFunc, Dict),
	S#environmentState{
		cars = NewCars,
		total_cars = New_Total_Cars,
		car_pids = NewDict
	}.

removeUserFromEnvironment(Pid, S) ->
	Users = S#environmentState.users,
	Dict = S#environmentState.user_pids,
	Total_users = S#environmentState.total_users,
	NewUsers = lists:delete(Pid, Users),
	New_Total_Users = Total_users - 1,
	FilterFunc = fun(_Key, Val) -> hd(Val) /= Pid end,
	NewDict = dict:filter(FilterFunc, Dict),
	S#environmentState{
		users = NewUsers,
		total_users = New_Total_Users,
		user_pids = NewDict
	}.

kill_user(Pid, ForceKill) ->
	Out = if 
		ForceKill -> 
			utente:die(Pid);
		true ->
			utente:dieSoft(Pid)
	end,
		if 
		Out == killed ->
			print_debug_message(self(), "User with PID {~w} removed", Pid);
		true ->
			print_debug_message(self(), "User with PID {~w} removed", Pid)
	end,
	Out.

changeDestUser(_Pid, _NewDest) ->
	%TODO check if user can change ad make it real
	changed.

% uccide l'orologio e gli automi delle macchine / utenti
terminate(State) ->
	Cars = State#environmentState.cars,
	Users = State#environmentState.users,
	killEntities(Cars, fun kill_car/2, true),
	killEntities(Users, fun kill_user/2, true),
	PID_clock = State#environmentState.tick_s_pid,
	PID_server_gps = State#environmentState.pid_gps_server,
	end_clock(PID_clock),
	end_gps_server(PID_server_gps),
    ok.

%% ====================================================================
%% Utilities functions
%% ====================================================================
getUpdatedTaxiName(S) -> 
	construct_string("~p~w", [S#environmentState.car_prefix, S#environmentState.last_car_id]).

getUpdatedUserName(S) -> 
	construct_string("~p~w", [S#environmentState.user_prefix, S#environmentState.last_user_id]).

updateLastCarName(S) ->
	LastID = S#environmentState.last_car_id,
	S#environmentState{last_car_id = LastID + 1}.

updateLastUserName(S) ->
	LastID = S#environmentState.last_user_id,
	S#environmentState{last_user_id = LastID + 1}.

updateRefs([], Pids_Dict, Pids_List) -> {Pids_Dict, Pids_List};
updateRefs([H | T], Pids_Dict, Pids_List) -> 
	{Name, Pid} = H,
	New_Pids_List = [Pid] ++ Pids_List,
	New_Pids_Dict = dict:append(Name, Pid, Pids_Dict),
	updateRefs(T, New_Pids_Dict, New_Pids_List).

getPidFromDict(Name, Dict) ->
	dict:find(Name, Dict).

searchCarNotCrashed(_S, _CrashedCars, false, Result) -> Result; 
searchCarNotCrashed(S, CrashedCars, true, _Result) -> 
	RandomCar = getRandomCar(S),
	Is_Member = lists:member(RandomCar, CrashedCars),
	searchCarNotCrashed(S, CrashedCars, Is_Member, RandomCar).

findAndKillCar(_S, killed, PidCarRemoved, _CurrentTries) -> PidCarRemoved; 
findAndKillCar(_S, not_killed, _PidCarRemoved, ?MAX_KILL_TRIES) -> -1; 
findAndKillCar(S, not_killed, _PidCarRemoved, CurrentTries) -> 
	RandomCar = getRandomCar(S),
	Out = kill_car(RandomCar, false),
	findAndKillCar(S, Out, RandomCar, CurrentTries + 1).

killEntities([], _KillFunc, _ForceKill) -> ok;
killEntities([H | T], KillFunc, ForceKill) -> 
	KillFunc(H, ForceKill),
	killEntities(T, KillFunc, ForceKill).

generate_entities(State, _Data, 0, _GenerateFunc, _UpdateNameFunct, Outs) -> {State, Outs};
generate_entities(State, Data, N, GenerateFunc, UpdateNameFunct, Outs) -> 
	Out = GenerateFunc(State),
	NewS = UpdateNameFunct(State, Data),
	New_Outs = [Out] ++ Outs,
	generate_entities(NewS, Data, N-1, GenerateFunc, UpdateNameFunct, New_Outs).

getRandomNode(S) -> getRandomNode(S, "").
getRandomNode(S, NodeName) -> 
	getRandomPositionName(S#environmentState.city#city.nodes, NodeName).

getRandomUser(S) ->
	getRandomEntity(S#environmentState.users, S#environmentState.total_users).

getRandomCar(S) ->
	getRandomEntity(S#environmentState.cars, S#environmentState.total_cars).

getRandomCrashedCar(S) ->
	getRandomEntity(S#environmentState.cars_crashed, S#environmentState.total_cars_crashed).

generateUserRequest(State, User_Position) ->
	{User_Position, getRandomNode(State, User_Position)}.