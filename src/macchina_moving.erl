-module(macchina_moving).
-compile(export_all).
-behaviour(gen_statem).

-define(HANDLE_COMMON,
    ?FUNCTION_NAME(T, C, D) -> handle_common(T, C,?FUNCTION_NAME, D)).

start() ->
	{ok, Pid} = gen_statem:start(?MODULE, foo, []),
	Pid.

callback_mode() ->
    [state_functions,state_enter].

init(foo) ->
	process_flag(trap_exit, true),
	tick_server:start(?TICKTIME, self()),
	Queue = [],
	{ok, idle, Queue}.

updateQueue(Pid, Queue) ->
    gen_statem:cast(Pid, {updateQueue, Queue}).

handle_common(cast, {updateQueue, RcvQueue}, _OldState, Queue) ->
	NewQueue = Queue ++ RcvQueue,
	my_util:println("ricevuto queue nuova"),
	{next_state, moving, NewQueue}.

idle(enter, _OldState, _Queue) ->
	my_util:println("sono in idle"),
	keep_state_and_data;
	
idle(info, {_From, tick}, _Queue) ->
	keep_state_and_data;
	?HANDLE_COMMON.
	  
moving(enter, _OldState, Queue) ->
	my_util:printList("situazione coda:",Queue),
	keep_state_and_data;
	
moving(info, {_From, tick}, Queue) ->
	my_util:println("ricevuto tick...aggiorno spostamento"),
	NewQueue = tl(Queue),
	if
		NewQueue =:= [] ->
			{next_state, idle, []};
		true ->
			my_util:printList("situazione coda:", NewQueue),
			{next_state, moving, NewQueue}
	end;
	?HANDLE_COMMON.


	