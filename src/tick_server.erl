-module(tick_server).
-import('send', [send_message/2, send_message/3]).
-import('utilities', [print_debug_message/1, print_debug_message/2, print_debug_message/3]).
-export([start_clock/1, end_clock/1, tick_generation/1]).
-include("globals.hrl").
%timeToTick: espresso in secondi, tempo di attesa tra due tick
%Subscribers: lista dei processi che ricevono il tick
start_clock(Subcribers) ->
	Pid = spawn_link(tick_server, tick_generation, [Subcribers]),
	Pid.

tick_generation(Subscribers) ->
	receive 
		{Pid, Ref, terminate} ->
			send_message(Pid, {Ref, ok}),
			print_debug_message(self(), "Exiting clock loop");
		Unknown ->
			print_debug_message(self(), "Tick Server Received Unknown: ~p", [Unknown]),
			tick_generation(Subscribers)

	after ?TICKTIME*1000 -> 
		send_notification(Subscribers),
		tick_generation(Subscribers)
	end.

send_notification([]) -> ok;
send_notification([H | T]) -> 
	send_message(H, self(), tick),
	send_notification(T).


end_clock(Pid) ->
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