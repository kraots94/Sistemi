% ciao
%modifica branch
-module(hello).
-import('my_util', [println/1]).
-export([hello_world/0]).

hello_world() ->
	my_util:println("Hello, World!").