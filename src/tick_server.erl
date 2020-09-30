-module(tick_server).
-import('send', [send_message/2, send_message/3]).
-import('my_util',[println/1]).
-export([start_clock/2, end_clock/1, tick_generation/2]).

%timeToTick: espresso in secondi, tempo di attesa tra due tick
%Subscribers: lista dei processi che ricevono il tick
start_clock(TimeToTick, Subcribers) ->
	Pid = spawn(tick_server, tick_generation, [TimeToTick, Subcribers]),
	Pid.

tick_generation(TimeToTick,Subscribers) ->
	receive 
		{Pid, Ref, terminate} ->
			send_message(Pid, {Ref, ok}),
			println("Exiting clock loop");
		Unknown ->
            io:format("Unknown message: ~p~n", [Unknown]),
			tick_generation(TimeToTick,Subscribers)
	after TimeToTick*1000 -> send_notification(Subscribers),
						tick_generation(TimeToTick,Subscribers)
	end.

send_notification([]) -> ok;
send_notification([H | T]) -> 
	send_message(H, self(), tick),
	send_notification(T).


end_clock(Pid) ->
	println("Killing clock."),
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