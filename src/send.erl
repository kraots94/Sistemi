-module(send).
-export([send_message/2, send_message/3]).

send_message(Pid, Message) ->
	Pid ! Message.

send_message(Pid, From, Message) ->
	Pid ! {From, Message}.