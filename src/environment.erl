-module(environment).

-export([start_link/0, end_environment/1]).

-import('tick_server',[start_clock/2, end_clock/1]).
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
	Pid_Wireless_Server = wireless_card_server:start_link(City#city.nodes),
	println("Started Server~n"), 
	S = #environmentState{cars = [], 
						  users = [], 
						  city = City, 
						  tick_s_pid = Pid_Tick, 
						  wireless_card_server_pid = Pid_Wireless_Server, 
						  tick_counter = 0},
	NewState = generate_taxi(S),
	NewState2 = generate_taxi(NewState),
	%NewState3 = generate_user(NewState2),
	%println("Stato: ", NewState3).
	loop(NewState2).

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
			Pid ! 12,
			loop(State);
		{_Pid, tick} -> CurrentTickCounter = State#environmentState.tick_counter,
						NextTick = CurrentTickCounter + 1,
						NewTickCounter =
							if NextTick >= ?TICKS_EVENT -> 
							  generate_event(my_util:generate_random_number(100)),
							  0;
							true -> NextTick
							end,
					    loop(State#environmentState{tick_counter = NewTickCounter});
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
generate_event(N) ->
	if 	N < 0 ->  println("nothing happened");
		N < 10 -> println("spawn car");
	   	N < 15 -> println("nothing happened");
	   	N < 25 -> println("spawn client");
	   	N < 35 -> println("nothing happened");
	   	N < 45 -> println("client change target");
	   	N < 50 -> println("nothing happened");
	   	N < 60 -> println("car crash");
	   	N < 70 -> println("fix car");
		N < 75 -> println("nothing happened");
		N < 77 -> println("add node to map");
		N == 77 -> println("nothing happened");
		N < 80 -> println("add street to map");
		N == 80 -> println("nothing happened");
		N < 83 -> println("remove node to map");
		N == 83 -> println("nothing happened");
		N < 86 -> println("remove arrow to map");
		N < 95 -> println("nothing happened");
		N < 100 -> println("remove car");
		true -> println("nothing happened")
	end.

getRandomName(S) -> 
	Nodes = S#environmentState.city#city.nodes,
	NodeName = getRandomPositionName(Nodes),
	NodeName.

generate_taxi(Stato) ->
	Name = getRandomName(Stato),
	PidTaxi = macchina_moving_withRecords:start(Name),
	NewCars = [PidTaxi] ++ Stato#environmentState.cars,
	Stato#environmentState{cars = NewCars}.

generateUserRequest(Stato, User_Position) ->
	{User_Position, getRandomName(Stato)}.

getRandomCar(S) ->
	Cars = S#environmentState.cars,
	Total_Cars = length(Cars),
	RandomN = my_util:generate_random_number(Total_Cars),
	RandomCar = lists:nth(RandomN, Cars),
	RandomCar.

generate_user(Stato) ->
	%genera richiesta {From, To} e posizioneInizialeUtente
	Name = getRandomName(Stato),
	Request = generateUserRequest(Stato, Name),
	PidUtente = appUtente_flusso:start(Name),
	Car = getRandomCar(Stato),
	appUtente_flusso:sendRequest(Car, PidUtente, Request),
	NewUsers = [PidUtente] ++ Stato#environmentState.users,
	Stato#environmentState{users = NewUsers}.

% uccide l'orologio e gli automi delle macchine / utenti
terminate(State) ->
	PID_clock = State#environmentState.tick_s_pid,
	end_clock(PID_clock),
%delete map
    ok.
