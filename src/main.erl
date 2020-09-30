-module(main).
-export([hello_world/0]).
-import('environment', [start_link/0]).

hello_world() ->
	io:format("Starting World!~n"),
	Pid_Environment = environment:start_link(),
	Pid_Environment.
	