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
						print_environment_message/3]).

-include("records.hrl").
-include("globals.hrl").
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
	print_debug_message(self(), "Environment Created", none),
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
        {Pid, Ref, terminate} ->
			send_message(Pid, {Ref, ok}),
			terminate(State),
			print_debug_message(self(), "Exiting enviroment loop", []);
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
			CarID = getRandomCar(S),
			print_environment_message(self(), "Car {~w} crashed!", [CarID]),
			S;
	   	
		N < 70 -> 
			print_debug_message(self(), "Event: ~p", "fix car"),
			S;
		
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
			S;
		
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

% uccide l'orologio e gli automi delle macchine / utenti
terminate(State) ->
	PID_clock = State#environmentState.tick_s_pid,
	PID_server_gps = State#environmentState.pid_gps_server,
	end_clock(PID_clock),
	end_gps_server(PID_server_gps),
    ok.

%% ====================================================================
%% Utilities functions
%% ====================================================================

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
	TotalUsers = S#environmentState.total_users,
	RandomUser = if
		TotalUsers > 0 ->
			Users = S#environmentState.users,
			RandomN = utilities:generate_random_number(TotalUsers),
			lists:nth(RandomN, Users);
		true ->
			-1
	end,
	RandomUser.

getRandomCar(S) ->
	Total_Cars = S#environmentState.total_cars,
	RandomCar = if
		Total_Cars > 0 ->
			Cars = S#environmentState.cars,
			RandomN = utilities:generate_random_number(Total_Cars),
			lists:nth(RandomN, Cars);
		true ->
			-1
	end,
	RandomCar.

generateUserRequest(State, User_Position) ->
	{User_Position, getRandomNode(State)}.