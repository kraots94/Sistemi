-module(send).
-export([send_message/3]).

send_message(Pid, From, Message) ->
	Pid ! {From, Message}.