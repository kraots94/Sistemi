-module(main).
-export([start_project/0]).
-import('environment', [start_link/0]).
-import('utilities', [print_debug_message/1, print_debug_message/2, print_debug_message/3]).

%f(), make:all(), PID_ENV = main:start_project().
% environment:end_environment(PID_ENV).
start_project() ->
	print_debug_message("Starting World!"),
	Pid_Environment = environment:start_link(),
	Pid_Environment.
	
