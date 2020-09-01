% ciao
%modifica branch

%test modifica da pc fisso
%mod 2 fisso
%mod 3 fisso qwdqwd qwdqwdqwd 

-module(hello).
-import('my_util', [println/1]).
-export([hello_world/0]).

hello_world() ->
	my_util:println("Hello, World!").