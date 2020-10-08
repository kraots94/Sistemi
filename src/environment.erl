-module(environment).

-export([start_link/0, end_environment/1]).

-import('tick_server',[start_clock/2, end_clock/1]).
-import('gps_server', [start_gps_server/1, end_gps_server/1]).
-import('send', [send_message/2, send_message/3]).
-import('city_map', [init_city/0]).
-import('nodes_util',[getRandomPositionName/1]).
-import('utilities', [print_debug_message/1, print_debug_message/2, print_debug_message/3]).

-include("records.hrl").
-include("globals.hrl").
% stato environment:

-record(environmentState,  {cars,
							total_cars,
							users, 
							total_users,
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
	print_debug_message("Start Environment"),
	City = init_city(),
	Pid_Tick = start_clock(?TICKTIME, [self()]),
	PID_server_gps = start_gps_server(City#city.nodes),
	S = #environmentState{cars = [], 
						  users = [], 
						  total_cars = 0,
						  total_users = 0,
						  city = City, 
						  tick_s_pid = Pid_Tick, 
						  pid_gps_server = PID_server_gps, 
						  tick_counter = 0},
	print_debug_message(self(), "Environment Created, state: ~w", [S]),

	loop(S).

loop(State) ->
    receive
		{Pid, tot_nodes} -> 
			Pid ! State#environmentState.city#city.total_nodes,
			loop(State);
		{_Pid, tick} -> CurrentTickCounter = State#environmentState.tick_counter,
						NextTick = CurrentTickCounter + 1,
						NewState =
							if NextTick >= ?TICKS_EVENT -> 
								  HandledState = handle_event(State, utilities:generate_random_number(100)),
								  HandledState#environmentState{tick_counter = 0};
							true -> 
								State#environmentState{tick_counter = NextTick}
							end,
					    loop(NewState);
		{_Pid, {event, N}} -> loop(handle_event(State, N));
		{_Pid, {print, wireless_card}} -> 
								State#environmentState.pid_gps_server ! {self(), {printState}},
								loop(State);
		{_Pid, {print, self}} -> 
								print_debug_message(self(), "Environment State: ~w", [State]),
								loop(State);
        {Pid, Ref, terminate} ->
			send_message(Pid, {Ref, ok}),
			terminate(State),
			print_debug_message(self(), "Exiting enviroment loop", []);
        Unknown ->
			print_debug_message(self(), "Environment Received Unknown: ~p", [Unknown]),
            loop(State)
    end.

%% ====================================================================
%% Handle Events functions
%% ====================================================================

handle_event(S, N) ->
	NewState = if 	
	    % Nothing
	    N < 0 ->  print_debug_message(self(), "Event: ~w", ["nothing happened"]),
				  S;
		% Spawn Car
		N < 10 -> print_debug_message(self(), "Event: ~w", ["spawn car"]),
				  generate_taxi(S);
					
	   	N < 15 -> print_debug_message(self(), "Event: ~w", ["nothing happened"]),
				  S;

		N < 25 -> print_debug_message(self(), "Event: ~w", ["spawn client"]),
				  generate_user(S);
	   	
		N < 35 -> print_debug_message(self(), "Event: ~w", ["nothing happened"]),
				  S;
	   	
		N < 45 -> print_debug_message(self(), "Event: ~w", ["client change target"]),
				  S;
	   	
		N < 50 -> print_debug_message(self(), "Event: ~w", ["nothing happened"]),
				  S;
	   	
		N < 60 -> print_debug_message(self(), "Event: ~w", ["car crash"]),
				  S;
	   	
		N < 70 -> print_debug_message(self(), "Event: ~w", ["fix car"]),
				  S;
		
		N < 75 -> print_debug_message(self(), "Event: ~w", ["nothing happened"]),
				  S;
		
		N < 77 -> print_debug_message(self(), "Event: ~w", ["add node to map"]),
				  S;
		
		N == 77 -> print_debug_message(self(), "Event: ~w", ["nothing happened"]),
				  S;
		
		N < 80 -> print_debug_message(self(), "Event: ~w", ["add street to map"]),
				  S;
		
		N == 80 -> print_debug_message(self(), "Event: ~w", ["nothing happened"]),
				  S;
		
		N < 83 -> print_debug_message(self(), "Event: ~w", ["remove node from map"]),
				  S;
		
		N == 83 -> print_debug_message(self(), "Event: ~w", ["nothing happened"]),
				  S;
		
		N < 86 -> print_debug_message(self(), "Event: ~w", ["remove street from map"]),
				  S;
		
		N < 95 -> print_debug_message(self(), "Event: ~w", ["nothing happened"]),
				  S;
		
		N < 100 -> print_debug_message(self(), "Event: ~w", ["remove car"]),
				  S;
		
		true -> print_debug_message(self(), "Event: ~w", ["nothing happened"]),
				  S
	end,
	NewState.

%% ====================================================================
%% Internal functions
%% ====================================================================

generate_taxi(Stato) ->
	StartingPos = getRandomNode(Stato),
	PidGpsServer = Stato#environmentState.pid_gps_server,
	City_Map = Stato#environmentState.pid_gps_server,
	PidTaxi = macchina_ascoltatore:start({StartingPos, PidGpsServer, City_Map}),
	NewCars = [PidTaxi] ++ Stato#environmentState.cars,
	Total_Cars = Stato#environmentState.total_cars + 1,
	Stato#environmentState{cars = NewCars, total_cars = Total_Cars}.

generate_user(Stato) ->
	NodeStart = getRandomNode(Stato),
	Request = generateUserRequest(Stato, NodeStart),
	PidGpsServer = Stato#environmentState.pid_gps_server,
	PidUtente = appUtente_flusso:start(NodeStart, PidGpsServer),
	appUtente_flusso:sendRequest(PidUtente, Request),
	NewUsers = [PidUtente] ++ Stato#environmentState.users,
	Total_Users = Stato#environmentState.total_users + 1,
	Stato#environmentState{users = NewUsers, total_users = Total_Users}.

% uccide l'orologio e gli automi delle macchine / utenti
terminate(State) ->
	PID_clock = State#environmentState.tick_s_pid,
	PID_server_gps = State#environmentState.pid_gps_server,
	end_clock(PID_clock),
	end_gps_server(PID_server_gps),
%delete map
    ok.

%% ====================================================================
%% Utilities functions
%% ====================================================================

getRandomNode(S) -> 
	Nodes = S#environmentState.city#city.nodes,
	NodeName = getRandomPositionName(Nodes),
	NodeName.

getRandomUser(S) -> 
	Users = S#environmentState.cars,
	TotalUsers = S#environmentState.total_users,
	RandomN = utilities:generate_random_number(TotalUsers),
	RandomUser = lists:nth(RandomN, Users),
	RandomUser.

getRandomCar(S) ->
	Cars = S#environmentState.cars,
	Total_Cars = S#environmentState.total_cars,
	RandomN = utilities:generate_random_number(Total_Cars),
	RandomCar = lists:nth(RandomN, Cars),
	RandomCar.

generateUserRequest(Stato, User_Position) ->
	{User_Position, getRandomNode(Stato)}.