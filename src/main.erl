-module(main).
-export([start_project/0]).
-import('environment', [start_link/0]).
-import('my_util', [init_random_generator/0, println/1]).

start_project() ->
	println("Starting World!~n"),
	init_random_generator(),
	Pid_Environment = environment:start_link(),
	Pid_Environment.
	