% Starting hello world with modules

-module(hello).
-import('my_util', [println/1]).
-export([hello_world/0, hello2/0, hello3/0,  hello4/0,  hello5/0, hello6/0]).

hello_world() ->
	my_util:println("Hello, World!").

hello2() ->
	io:format("~s~n",[<<"Hello">>]).

hello3() ->
	io:format("~p~n",[<<"Hello">>]).

hello4() ->
	io:format("~~~n").

hello5() ->
	io:format("~f~n", [4.0]).

hello6() ->
	io:format("~30f~n", [4.0]).