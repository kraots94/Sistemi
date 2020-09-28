%fsm di una cassaforte configurabile con codice lock e che si apre inserendo il codice
%per inserire il codice si deve chiamare down e up (premere il pulsate) x ogni digit, così facendo viene inserito il codice
-module(fsm_cassafort).
-behaviour(gen_statem).
-define(NAME, code_lock_2).

-export([start_link/1,stop/0]).
-export([down/1,up/1,code_length/0]).
-export([init/1,callback_mode/0,terminate/3]).
-export([locked/3,open/3]).

%fai partire il fsm e binda il nome del modulo al nome del processo che esegue
start_link(Code) ->
    gen_statem:start_link({local,?NAME}, ?MODULE, Code, []).

stop() ->
    gen_statem:stop(?NAME).

down(Button) ->
    gen_statem:cast(?NAME, {down,Button}).
up(Button) ->
    gen_statem:cast(?NAME, {up,Button}).
code_length() ->
    gen_statem:call(?NAME, code_length).

init(Code) ->
    process_flag(trap_exit, true),
    Data = #{code => Code, length => length(Code), buttons => []},
    {ok, locked, Data}. %vai in stato locked (prima nell'enter state) con stato Data (la map)

%config options, state_enter attiva gli enter events 
callback_mode() ->
    [state_functions,state_enter].
%per gestire velocemente eventi globali
-define(HANDLE_COMMON,
    ?FUNCTION_NAME(T, C, D) -> handle_common(T, C, D)).

%eventi globali
handle_common(cast, {down,Button}, Data) ->
    {keep_state, Data#{button => Button}}; %aggiungi alla map nuovo binding, ossia che bottone premuto è Button

handle_common(cast, {up,Button}, Data) ->
    case Data of
        #{button := Button} -> %prima è stato chiamato down con questo Button?
            {keep_state, maps:remove(button, Data),
             [{next_event,internal,{button,Button}}]}; %fai scattare controllo codice
        #{} ->
            keep_state_and_data
    end;

%funzione sincrona , richiamabile sempre -> evento globale
handle_common({call,From}, code_length, #{code := Code}) ->
    {keep_state_and_data,
     [{reply,From,length(Code)}]}.

%funzione state enter, la prima che parte quando entri nello stato
open(enter, _OldState, _Data) ->
    do_unlock(),
    {keep_state_and_data,
     [{state_timeout,10000,lock}]}; % Time in milliseconds, faccio rimanere aperta sto tempo qua

open(state_timeout, lock, Data) ->
    {next_state, locked, Data}; %timer apertura fires, devo chiudere

open(internal, {button,_}, _) ->
	{keep_state_and_data, [postpone]};
?HANDLE_COMMON.

do_lock() ->
    io:format("Locked~n", []).

do_unlock() ->
    io:format("Open~n", []).

terminate(_Reason, State, _Data) ->
    State =/= locked andalso do_lock(),
    ok.

%funzione state enter, la prima che parte quando entri nello stato
locked(enter, _OldState, Data) ->
    do_lock(),
    {keep_state, Data#{buttons := []}};

locked(state_timeout, button, Data) ->
	my_util:println("devi essere piu' veloce cazzo!"),
    {keep_state, Data#{buttons := []}};

%controllo che bottoni premuti (sempre tramite chiamata down + up) siano = al codice, se si apri
locked(
  internal, {button,Button},
  #{code := Code, length := Length, buttons := Buttons} = Data) ->
	my_util:println("hai premuto un pulsante"),
    NewButtons =
        if
            length(Buttons) < Length ->
                Buttons;
            true ->
                tl(Buttons)
        end ++ [Button],
    if
        NewButtons =:= Code -> % Correct
            {next_state, open, Data};
	true -> % Incomplete | Incorrect
            {keep_state, Data#{buttons := NewButtons},
             [{state_timeout,20000,button}]} % Time in milliseconds
    end;
?HANDLE_COMMON.
  