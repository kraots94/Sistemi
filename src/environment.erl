-module(environment).
-export([generate_event/1, generate_random_number/0, start_link/0, get_nearest/1, get_cars/2, end_environment/1]).
-import('tick_server',[start/2, end_clock/1]).
-import('my_util',[println/1]).
-import('send', [send_message/2, send_message/3]).
-record(state, {cars, users, map, tick_s_pid}).
-define(TICKTIME, 2).


%%% Client API
start_link() -> spawn_link(fun init/0).

%%% Server functions
init() -> 
	println("Start Environment~n"), 
	Pid_Tick = tick_server:start(?TICKTIME, [self()]),
	S = #state{cars = [], users = [], map = [], tick_s_pid = Pid_Tick},
	{T_M, T_S, T_m} = erlang:timestamp(),
	io:format("Current Timestamp: ~w ~w ~w ~n", [T_M, T_S, T_m]),
	rand:seed(exro928ss, {T_M, T_S, T_m}),
	loop(S).

end_environment(Pid) ->
	println("Killing Environment"),
	Ref = erlang:monitor(process, Pid),
	send:send_message(Pid, {self(), Ref, terminate}),
	
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
		{_Pid, tick} -> io:format("Tick received"),
							  generate_event(generate_random_number()),
							  loop(State);
        {Pid, Ref, terminate} ->
			send:send_message(Pid, {Ref, ok}),
			terminate(State),
			println("Exiting enviroment loop~n");
        Unknown ->
            %% do some logging here too
            io:format("Unknown message: ~p~n", [Unknown]),
            loop(State)
    end.

%%% Private functions
generate_event(N) ->
	if 	N < 0 ->  io:format("nothing happened~n");
		N < 10 -> io:format("spawn car~n");
	   	N < 15 -> io:format("nothing happened~n");
	   	N < 25 -> io:format("spawn client~n");
	   	N < 35 -> io:format("nothing happened~n");
	   	N < 45 -> io:format("client change target~n");
	   	N < 50 -> io:format("nothing happened~n");
	   	N < 60 -> io:format("car crash~n");
	   	N < 70 -> io:format("fix car~n");
		N < 75 -> io:format("nothing happened~n");
		N < 77 -> io:format("add node to map~n");
		N == 77 -> io:format("nothing happened~n");
		N < 80 -> io:format("add street to map~n");
		N == 80 -> io:format("nothing happened~n");
		N < 83 -> io:format("remove node to map~n");
		N == 83 -> io:format("nothing happened~n");
		N < 86 -> io:format("remove arrow to map~n");
		N < 95 -> io:format("nothing happened~n");
		N < 100 -> io:format("remove car~n");
		true -> io:format("nothing happened~n")
	end.

get_nearest(_Coord) -> ok.

get_cars(_Coord, _Range) -> ok.

generate_random_number() ->
	N = rand:uniform(100),
	io:format("Generated number: ~w ~n", [N]),
	N.

terminate(State) ->
	PID_clock = State#state.tick_s_pid,
	tick_server:end_clock(PID_clock),
    ok.