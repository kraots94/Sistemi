-module(main).
-export([hello_world/0]).
-import('environment', [start/0, generate_event/1, generate_random_number/0]).

hello_world() ->
	io:format("Starting Environment!~n"),
	environment:start(),
	environment:generate_event(environment:generate_random_number()),
	environment:generate_event(environment:generate_random_number()),
	environment:generate_event(environment:generate_random_number()),
	environment:generate_event(environment:generate_random_number()),
	environment:generate_event(environment:generate_random_number()),
	environment:generate_event(environment:generate_random_number()),
	environment:generate_event(environment:generate_random_number()),
	environment:generate_event(environment:generate_random_number()),
	environment:generate_event(environment:generate_random_number()),
	environment:generate_event(environment:generate_random_number()),
	environment:generate_event(environment:generate_random_number()),
	environment:generate_event(environment:generate_random_number()),
	environment:generate_event(environment:generate_random_number()),
	environment:generate_event(environment:generate_random_number()),
	environment:generate_event(environment:generate_random_number()),
	environment:generate_event(environment:generate_random_number()),
	environment:generate_event(environment:generate_random_number()),
	environment:generate_event(environment:generate_random_number()),
	environment:generate_event(environment:generate_random_number()),
	environment:generate_event(environment:generate_random_number()),
	environment:generate_event(environment:generate_random_number()).
	