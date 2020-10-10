-module(environment).

-export([start_link/0, end_environment/1]).

-import('tick_server',[start_clock/1, end_clock/1]).
-import('gps_server', [start_gps_server/1, end_gps_server/1]).
-import('send', [send_message/2, send_message/3]).
-import('city_map', [init_city/0]).
-import('nodes_util',[getRandomPositionName/1]).
-import('utilities', [print_debug_message/1, 
						print_debug_message/2, 
						print_debug_message/3,
						print_environment_message/1,
						print_environment_message/2,
						print_environment_message/3,
						getRandomEntity/2]).

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
							tick_counter}).
						
%% ====================================================================
%% API functions
%% ====================================================================
%%% Client API
start_link() -> spawn_link(fun init/0).

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
	print_environment_message("Start Environment"),
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
						  tick_counter = 0},
	print_debug_message(self(), "Environment Created"),
	loop(S).

loop(State) ->
    receive
		{_Pid, tick} -> 
			CurrentTickCounter = State#environmentState.tick_counter,
			NextTick = CurrentTickCounter + 1,
			NewState =
				if NextTick >= ?TICKS_EVENT -> 
						HandledState = handle_event(State, utilities:generate_random_number(100)),
						HandledState#environmentState{tick_counter = 0};
				true -> 
					State#environmentState{tick_counter = NextTick}
				end,
			loop(NewState);
		{event, N} -> 
			loop(handle_event(State, N));
		{print, wireless_card} -> 
			State#environmentState.pid_gps_server ! {self(), {printState}},
			loop(State);
		{print, self} -> 
			print_environment_message(self(), "Environment State: ~w", State),
			loop(State);
		{spawn, cars, N} -> 	
			print_debug_message(self(), "Have to spawn ~w cars", N),
			NewState = generate_multiple_cars(State, N),
			loop(NewState);
		{spawn, users, N} -> 
			print_debug_message(self(), "Have to spawn ~w users", N),
			NewState = generate_multiple_users(State, N),
			loop(NewState);
		{task_complete, user, Pid} -> 	
			print_debug_message(self(), "Have to remove user with Pid {~w}", Pid),
			NewState = remove_user_from_state(State, Pid),
			loop(NewState);
		{task_complete, car, Pid} -> 	
			print_debug_message(self(), "Have to remove car with Pid {~w}", Pid),
			NewState = remove_car_from_state(State, Pid),
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

handle_event(S, N) ->
	NewState = if 	
	    % Nothing
	    N < 0 ->  
			print_debug_message(self(), "Event: ~p", "nothing happened"),
			S;
		% Spawn Car
		N < 10 -> 
			print_debug_message(self(), "Event: ~p", "spawn car"),
			generate_single_car(S);
					
	   	N < 15 -> 
			print_debug_message(self(), "Event: ~p", "nothing happened"),
			S;

		N < 25 -> 
			print_debug_message(self(), "Event: ~p", "spawn client"),
			generate_single_user(S);
	   	
		N < 35 -> 
			print_debug_message(self(), "Event: ~p", "nothing happened"),
			S;
	   	
		N < 45 -> 
			print_debug_message(self(), "Event: ~p", "client change target"),
			Pid_User = getRandomUser(S),
			NewDestination = getRandomNode(S),
			print_environment_message(self(), "User {~w} has now new target [~p]", [Pid_User, NewDestination]),
			S;
		
		N < 50 -> 
			print_debug_message(self(), "Event: ~p", "nothing happened"),
			S;
		
		N < 60 -> 
			print_debug_message(self(), "Event: ~p", "car crash"),
			Total_Cars = S#environmentState.total_cars,
			Total_Crashed = S#environmentState.total_cars_crashed,
			if
				Total_Cars == Total_Crashed ->
					print_debug_message(self(), "Event car crash not occurred because all cars are already crashed"),
					S;
				true ->					
					CrashedCars = S#environmentState.cars_crashed,
					Car_To_Crash = searchCarNotCrashed(S, CrashedCars, true, -1),
					New_crashed_cars = [Car_To_Crash] ++ CrashedCars,
					New_total_cars_crashed = Total_Crashed + 1,
					%TODO Write here code to notify car

					print_environment_message(self(), "Car {~w} crashed!", Car_To_Crash),
					S#environmentState{
						cars_crashed = New_crashed_cars,
						total_cars_crashed = New_total_cars_crashed
					}
			end;
	   	
		N < 70 -> 
			print_debug_message(self(), "Event: ~p", "fix car"),
			Total_Crashed = S#environmentState.total_cars_crashed,
			if
				Total_Crashed == 0 ->
					print_debug_message(self(), "Event car fix not occurred because no cars are crashed"),
					S;
				true ->					
					Car_To_Fix = getRandomCrashedCar(S),
					CrashedCars = S#environmentState.cars_crashed,
					New_crashed_cars = lists:delete(Car_To_Fix, CrashedCars),
					New_total_cars_crashed = Total_Crashed - 1,
					%TODO Write here code to notify car
				
					print_environment_message(self(), "Car {~w} fixed!", Car_To_Fix),
					S#environmentState{
						cars_crashed = New_crashed_cars,
						total_cars_crashed = New_total_cars_crashed
					}
			end;

		N < 75 -> 
			print_debug_message(self(), "Event: ~p", "nothing happened"),
			S;
		
		N < 77 -> 
			print_debug_message(self(), "Event: ~p", "add node to map"),
			S;
		
		N == 77 -> 
			print_debug_message(self(), "Event: ~p", "nothing happened"),
			S;
		
		N < 80 -> 
			print_debug_message(self(), "Event: ~p", "add street to map"),
			S;
		
		N == 80 -> 
			print_debug_message(self(), "Event: ~p", "nothing happened"),
			S;
		
		N < 83 -> 
			print_debug_message(self(), "Event: ~p", "remove node from map"),
			S;
		
		N == 83 -> 
			print_debug_message(self(), "Event: ~p", "nothing happened"),
			S;
		
		N < 86 -> 
			print_debug_message(self(), "Event: ~p", "remove street from map"),
			S;
		
		N < 95 -> 
			print_debug_message(self(), "Event: ~p", "nothing happened"),
			S;
		
		N < 100 -> 
			print_debug_message(self(), "Event: ~p", "remove car"),
			Total_Cars = S#environmentState.total_cars,
			if 
				Total_Cars == 0 ->
					print_debug_message(self(), "Event kill car not occurred because no cars are "),
					S;
				true ->
					Pid_Removed = findAndKillCar(S, not_killed, -1, 0),
					if Pid_Removed == -1 ->
							print_debug_message(self(), "Event kill car not occurred because not found car in idle");
						true ->
							Cars = S#environmentState.cars,
							NewCars = lists:delete(Pid_Removed, Cars),
							New_Total_Cars = Total_Cars - 1,
							S#environmentState{
								cars = NewCars,
								total_cars = New_Total_Cars
							}
					end
			end;
		
		true -> 
			print_debug_message(self(), "Event: ~p", "nothing happened"),
			S
	end,
	NewState.

%% ====================================================================
%% Internal functions
%% ====================================================================

generate_multiple_cars(State, N) ->
	Cars_Generated = generate_entities(State, N, fun generate_taxi/1, []),
	NewCars = Cars_Generated ++ State#environmentState.cars,
	Total_Cars = State#environmentState.total_cars + N,
	State#environmentState{cars = NewCars, total_cars = Total_Cars}.

generate_multiple_users(State, N) -> 
	Users_Generated = generate_entities(State, N, fun generate_user/1, []),
	NewUsers = Users_Generated ++ State#environmentState.users,
	Total_Users = State#environmentState.total_users + N,
	State#environmentState{users = NewUsers, total_users = Total_Users}.
	
generate_single_car(State) ->
	PidTaxi = generate_taxi(State),
	NewCars = [PidTaxi] ++ State#environmentState.cars,
	Total_Cars = State#environmentState.total_cars + 1,
	State#environmentState{cars = NewCars, total_cars = Total_Cars}.

generate_single_user(State) ->
	PidUtente = generate_user(State),
	NewUsers = [PidUtente] ++ State#environmentState.users,
	Total_Users = State#environmentState.total_users + 1,
	State#environmentState{users = NewUsers, total_users = Total_Users}.

generate_taxi(State) ->
	StartingPos = getRandomNode(State),
	PidGpsServer = State#environmentState.pid_gps_server,
	City_Map = State#environmentState.city,
	PidTaxi = macchina_ascoltatore:start({StartingPos, PidGpsServer, City_Map}),
	PidTaxi.

generate_user(State) ->
	NodeStart = getRandomNode(State),
	Request = generateUserRequest(State, NodeStart),
	PidGpsServer = State#environmentState.pid_gps_server,
	PidUtente = appUtente_flusso:start({NodeStart, PidGpsServer}),
	appUtente_flusso:sendRequest(PidUtente, Request),
	PidUtente.

kill_car(Pid, ForceKill) ->
	Out = if 
		ForceKill -> 
			macchina_ascoltatore:die(Pid);
		true ->
			macchina_ascoltatore:dieSoft(Pid)
	end,
	if 
		Out == killed ->
			print_environment_message(self(), "Car with PID {~w} removed", Pid);
		true ->
			print_environment_message(self(), "Car with PID {~w} removed", Pid)
	end,
	Out.

kill_user(Pid, ForceKill) -> %TODO Notify user
	Out = if 
		ForceKill -> 
%			utente:die(Pid),
			killed;
		true ->
%			utente:dieSoft(Pid),
			killed
	end,
		if 
		Out == killed ->
			print_environment_message(self(), "User with PID {~w} removed", Pid);
		true ->
			print_environment_message(self(), "User with PID {~w} removed", Pid)
	end,
	Out.

remove_user_from_state(State, Pid) ->
	NewUsers = lists:delete(Pid, State#environmentState.users),
	Total_Users = State#environmentState.total_users - 1,
	State#environmentState{users = NewUsers, total_users = Total_Users}.

remove_car_from_state(State, Pid) ->
	NewCars = lists:delete(Pid, State#environmentState.cars),
	Total_Cars = State#environmentState.total_cars - 1,
	State#environmentState{users = NewCars, total_cars = Total_Cars}.

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

generate_entities(_State, 0, _GenerateFunc, PIDS) -> PIDS;
generate_entities(State, N, GenerateFunc, PIDS) -> 
	NewPid = GenerateFunc(State),	
	New_Pids = [NewPid]	++ PIDS,
	generate_entities(State, N-1, GenerateFunc, New_Pids).

getRandomNode(S) -> 
	Nodes = S#environmentState.city#city.nodes,
	NodeName = getRandomPositionName(Nodes),
	NodeName.

getRandomUser(S) ->
	getRandomEntity(S#environmentState.users, S#environmentState.total_users).

getRandomCar(S) ->
	getRandomEntity(S#environmentState.cars, S#environmentState.total_cars).

getRandomCrashedCar(S) ->
	getRandomEntity(S#environmentState.cars_crashed, S#environmentState.total_cars_crashed).

generateUserRequest(State, User_Position) ->
	{User_Position, getRandomNode(State)}.