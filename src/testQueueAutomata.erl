-module(testQueueAutomata).
-compile(export_all).
-behaviour(gen_statem).
-include("records.hrl").
-include("globals.hrl").

callback_mode() -> [state_functions].

inviaRobaComeAutoma(Pid, Roba) ->
	gen_statem:cast(Pid, Roba).
	
start(foo) ->
	{ok, Pid} = gen_statem:start_link(?MODULE,foo, []),
	Pid.

init(foo) ->
	{ok, idle, foo}.

%info = ricevo da un processo non automa
idle(info, Something, _S) ->
	utilities:println("ho ricevuto", Something),
	utilities:print_debug_message(self(), [intero], "pene"),
	timer:sleep(10000),
	utilities:println("uscito dallo sleep"),
	keep_state_and_data;

idle(cast, Something, _S) ->
	utilities:println("ho ricevuto come cast", Something),
	timer:sleep(10000),
	utilities:println("uscito dallo sleep"),
	keep_state_and_data.