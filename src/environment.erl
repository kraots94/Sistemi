-module(environment).

-export([start_link/0, end_environment/1, getRandomCar/1]).

-import('tick_server',[start_clock/2, end_clock/1]).
-import('wireless_card_server', [start_wireless_server/1, end_wireless_server/1]).
-import('my_util',[println/1, println/2]).
-import('send', [send_message/2, send_message/3]).
-import('city_map', [init_city/0]).
-import('nodes_util',[getRandomPositionName/1]).

-include("records.hrl").
-include("globals.hrl").

%%% Client API
start_link() -> spawn_link(fun init/0).

%%% Server functions
init() -> 
	println("Start Environment~n"), 
	City = init_city(),
	Pid_Tick = start_clock(?TICKTIME, [self()]),
	println("Start Wireless server"), 
	Pid_Wireless_Server = start_wireless_server(City#city.nodes),
	println("Started Server~n"), 
	S = #environmentState{cars = [], 
						  users = [], 
						  city = City, 
						  tick_s_pid = Pid_Tick, 
						  wireless_card_server_pid = Pid_Wireless_Server, 
						  tick_counter = 0},
	loop(S).

end_environment(Pid) ->
	println("Killing Environment"),
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

loop(State) ->
    receive
		{Pid, tot_nodes} -> 
			Pid ! State#environmentState.city#city.total_nodes,
			loop(State);
		{_Pid, tick} -> CurrentTickCounter = State#environmentState.tick_counter,
						NextTick = CurrentTickCounter + 1,
						NewState =
							if NextTick >= ?TICKS_EVENT -> 
								  HandledState = handle_event(State, my_util:generate_random_number(100)),
								  HandledState#environmentState{tick_counter = 0};
							true -> 
								State#environmentState{tick_counter = NextTick}
							end,
					    loop(NewState);
		{_Pid, {event, N}} -> loop(handle_event(State, N));
		{_Pid, {print, wireless_card}} -> 
								State#environmentState.wireless_card_server_pid ! {self(), {printState}},
								loop(State);
		{_Pid, {print, self}} -> 
								println("ENV state: ", State),
								loop(State);
        {Pid, Ref, terminate} ->
			send_message(Pid, {Ref, ok}),
			terminate(State),
			println("Exiting enviroment loop~n");
        Unknown ->
            %% do some logging here too
            io:format("Unknown message: ~p~n", [Unknown]),
            loop(State)
    end.

%%% Private functions
handle_event(S, N) ->
	NewState = if 	
	    % Nothing
	    N < 0 ->  println("nothing happened"), 
				  S;
		% Spawn Car
		N < 10 -> println("spawn car"),
				  generate_taxi(S);
					
	   	N < 15 -> println("nothing happened"),
				  S;

		N < 25 -> println("spawn client"),
				  generate_user(S);
	   	
		N < 35 -> println("nothing happened"),
				  S;
	   	
		N < 45 -> println("client change target"),
				  S;
	   	
		N < 50 -> println("nothing happened"),
				  S;
	   	
		N < 60 -> println("car crash"),
				  S;
	   	
		N < 70 -> println("fix car"),
				  S;
		
		N < 75 -> println("nothing happened"),
				  S;
		
		N < 77 -> println("add node to map"),
				  S;
		
		N == 77 -> println("nothing happened"),
				  S;
		
		N < 80 -> println("add street to map"),
				  S;
		
		N == 80 -> println("nothing happened"),
				  S;
		
		N < 83 -> println("remove node to map"),
				  S;
		
		N == 83 -> println("nothing happened"),
				  S;
		
		N < 86 -> println("remove arrow to map"),
				  S;
		
		N < 95 -> println("nothing happened"),
				  S;
		
		N < 100 -> println("remove car"),
				  S;
		
		true -> println("nothing happened"),
				  S
	end,
	NewState.

getRandomNode(S) -> 
	Nodes = S#environmentState.city#city.nodes,
	NodeName = getRandomPositionName(Nodes),
	NodeName.

generate_taxi(Stato) ->
	StartingPos = getRandomNode(Stato),
	PidWireless = Stato#environmentState.wireless_card_server_pid,
	PidTaxi = macchina_moving_withRecords:start(StartingPos, PidWireless),
	NewCars = [PidTaxi] ++ Stato#environmentState.cars,
	Stato#environmentState{cars = NewCars}.

generateUserRequest(Stato, User_Position) ->
	{User_Position, getRandomNode(Stato)}.

getRandomCar(S) ->
	Cars = S#environmentState.cars,
	Total_Cars = length(Cars),
	RandomN = my_util:generate_random_number(Total_Cars),
	RandomCar = lists:nth(RandomN, Cars),
	RandomCar.

generate_user(Stato) ->
	NodeStart = getRandomNode(Stato),
	Request = generateUserRequest(Stato, NodeStart),
	PidEnv = self(),
	PidWireless = Stato#environmentState.wireless_card_server_pid,
	PidUtente = appUtente_flusso:start(NodeStart, PidEnv, PidWireless),
	appUtente_flusso:sendRequest(Request),
	NewUsers = [PidUtente] ++ Stato#environmentState.users,
	Stato#environmentState{users = NewUsers}.

% uccide l'orologio e gli automi delle macchine / utenti
terminate(State) ->
	PID_clock = State#environmentState.tick_s_pid,
	PID_wireless_server = State#environmentState.wireless_card_server_pid,
	end_clock(PID_clock),
	end_wireless_server(PID_wireless_server),
%delete map
    ok.
