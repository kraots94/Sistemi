-module(tick).
-import(send, [send_message/3]).
-record(state, {target_pid,
	to_go}).
-export([init/2]).

init(Target, Time) ->
	S = #state{target_pid = Target, to_go = Time},
	tick(S).

tick(S) ->
	receive
	after S#state.to_go -> send_message(S#state.target_pid, self(), "Wake up")
	end.