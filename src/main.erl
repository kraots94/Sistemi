-module(main).
-export([start_project/0]).
-import('environment', [start_link/0]).
-import('utilities', [print_message_raw/1]).

%f(), make:all(), PID_ENV = main:start_project().
% environment:end_environment(PID_ENV).
start_project() ->
	print_message_raw("Starting Project~n"),
	print_message_raw("Devs: Alessandro, Angelo~n"),
	print_message_raw("Mails: forgiarini.alessandro@spes.uniud.it, andreussi.angelo@spes.uniud.it~n"),
	Pid_Environment = environment:start_link(),
	Pid_Environment.
	
