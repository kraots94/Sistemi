-module(macchina_moving_withRecords).
-compile(export_all).
-behaviour(gen_statem).
-define(TICKTIME, 2).
-define(HANDLE_COMMON,
    ?FUNCTION_NAME(T, C, D) -> handle_common(T, C,?FUNCTION_NAME, D)).

-record(tappa, {user = "",
				tipo = "",
				tempo = 0,
				posizione = {0,0}
				}).

println(Foo) ->
	io:format("~p~n", [Foo]).
printList(List) ->
	io:format("~w~n", [List]). %il w non stampa lista in forma di stringa

start() ->
	{ok, Pid} = gen_statem:start(?MODULE, foo, []),
	Pid.

updateQueue(Pid, Queue) ->
    gen_statem:cast(Pid, {updateQueue, Queue}).


init(foo) ->
	process_flag(trap_exit, true),
	tick_server:start(?TICKTIME, self()),
	Queue = [],
	{ok, idle, Queue}.

callback_mode() ->
    [state_functions,state_enter].

handle_common(cast, {updateQueue, RcvQueue}, _OldState, Queue) ->
	NewQueue = Queue ++ RcvQueue,
	println("ricevuto queue nuova"),
	{next_state, moving, NewQueue}.

idle(enter, _OldState, _Queue) ->
	println("sono in idle"),
	keep_state_and_data;
	
idle(info, {_From, tick}, _Queue) ->
	keep_state_and_data;
	?HANDLE_COMMON.
	  
moving(enter, _OldState, Queue) ->
	println("situazione coda:"),
	printList(Queue),
	keep_state_and_data;
	
moving(info, {_From, tick}, Queue) ->
	println("ricevuto tick..aggiorno spostamento"),
	NewQueue = tl(Queue),
	if
		NewQueue =:= [] ->
			{next_state, idle, []};
		true ->
			println("situazione coda:"),
			printList(NewQueue),
			{next_state, moving, NewQueue}
	end;
	?HANDLE_COMMON.


	