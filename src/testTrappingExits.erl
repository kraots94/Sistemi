-module(testTrappingExits).
-compile(export_all).

%per ottenere oggetto Pid dalla stringa del pid usa: 'list_to_pid(..)'
start_critic2() ->
	PidRest = spawn(?MODULE, restarter, []),
	io:format("restarter: ~p~n", [PidRest]).

restarter() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, critic2, []),
    io:format("critico: ~p~n", [Pid]),
	register(critic, Pid),
    receive
        {'EXIT', Pid, normal} -> % not a crash
            ok;
        {'EXIT', Pid, shutdown} -> % manual termination, not a crash
            io:format("ricevuto segnale terminazione, ora spengo tutto~n"),
			ok;
        {'EXIT', Pid, _} ->
            restarter();
		M -> 
			io:format("qualche processo mi sta uccidendo! (shell maybe) ,msg ricevuto: ~p!!",[M])
	end. 

judge2(Band, Album) ->
    Ref = make_ref(),
    critic ! {self(), Ref, {Band, Album}},
    receive
        {Ref, Criticism} -> Criticism
    after 2000 ->
        timeout
    end.

spegniCritico() -> 
	critic ! shutDwn.
	
critic2() ->
    receive
        {From, Ref, {"Rage Against the Turing Machine", "Unit Testify"}} ->
            From ! {Ref, "They are great!"};
        {From, Ref, {"System of a Downtime", "Memoize"}} ->
            From ! {Ref, "They're not Johnny Crash but they're good."};
        {From, Ref, {"Johnny Crash", "The Token Ring of Fire"}} ->
            From ! {Ref, "Simply incredible."};
        {From, Ref, {_Band, _Album}} ->
            From ! {Ref, "They are terrible!"};
		shutDwn -> 
			io:format("branch uscita...~n"),
			exit(self(), shutdown)
    end,
    critic2().




