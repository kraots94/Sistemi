-module(tick_server).
-import(send, [send_message/3]).
-compile(export_all).

%timeToTick espresso in secondi, Subscriber = chi riceve il tick
start(TimeToTick, Subcriber) ->
	Pid = spawn(tick_server, tick_generation, [TimeToTick,Subcriber]),
	Pid.

tick_generation(TimeToTick,Subscriber) ->
	receive 
	after TimeToTick*1000 -> send_message(Subscriber, self(), tick),
						tick_generation(TimeToTick,Subscriber)
	end.
