-module(environment).
-export([generate_event/1, start_link/0, get_nearest/1, get_cars/2, end_environment/1]).
-import('tick_server',[start_clock/2, end_clock/1]).
-import('my_util',[println/1]).
-import('send', [send_message/2, send_message/3]).
-import('graph', [from_file/1, num_of_edges/1, num_of_vertices/1, del_graph/1]).
-record(state, {cars, users, map, tick_s_pid}).
-define(TICKTIME, 2).

%%% Client API
start_link() -> spawn_link(fun init/0).

%%% Server functions
init() -> 
	println("Start Environment~n"), 
	City_Map = from_file("map\\city_map_graph.dat"),
	io:format("Total nodes: ~w~n", [num_of_vertices(City_Map)]),
	io:format("Total edges: ~w~n", [num_of_edges(City_Map)]),
	del_graph(City_Map),
	Pid_Tick = start_clock(?TICKTIME, [self()]),
	S = #state{cars = [], users = [], map = [], tick_s_pid = Pid_Tick},
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
		{_Pid, tick} -> println("Tick received"),
							  generate_event(my_util:generate_random_number()),
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

get_nearest(_Coord) -> ok.

get_cars(_Coord, _Range) -> ok.

% uccide l'orologio e gli automi delle macchine / utenti
terminate(State) ->
	PID_clock = State#state.tick_s_pid,
	end_clock(PID_clock),
    ok.