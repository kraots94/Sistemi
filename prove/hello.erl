% Programma Hello wolrd
% module -> come i package di Java
-module(hello).

% Funzione esportata, quindi resa pubblica. /0 indica che la funzione ha arietà 0, 0 parametri
-export([hello_world/0]).

% nome funzione -> implementazione
hello_world() ->
    % modulo io : funzione format/2 i paramentri sono la stringa e gli argomenti
    io:format("Hello, Wolrd!~n", []).