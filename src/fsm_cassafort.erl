%fsm di una cassaforte configurabile con codice lock e che si apre inserendo il codice
%per inserire il codice si deve chiamare down e up (premere il pulsate) x ogni digit, così facendo viene inserito il codice
-module(fsm_cassafort).
-behaviour(gen_statem).
-define(NAME, fsm_cassafort).
-define(TICKTIME, 10).
%per gestire velocemente eventi globali
-define(HANDLE_COMMON,
    ?FUNCTION_NAME(T, C, D) -> handle_common(T, C,?FUNCTION_NAME, D)).

-export([start_link/1,stop/0,start/1]).
-export([down/1,up/1,code_length/0,code_length_pid/1]).
-export([init/1,callback_mode/0,terminate/3]).
-export([locked/3,open/3]).

%fai partire il fsm e binda il nome del modulo al nome del processo che esegue
start_link(Code) ->
    gen_statem:start_link({local,?NAME}, ?MODULE, Code, []).

start(Code) ->
	{ok, Pid} = gen_statem:start(?MODULE, Code, []),
	Pid.

stop() ->
    gen_statem:stop(?NAME).

down(Button) ->
    gen_statem:cast(?NAME, {down,Button}).
up(Button) ->
    gen_statem:cast(?NAME, {up,Button}).
code_length() ->
    gen_statem:call(?NAME, code_length).

code_length_pid(To) ->
	gen_statem:call(To, code_length).

init(Code) ->
    process_flag(trap_exit, true),
	TickServ = tick_server:start(?TICKTIME, self()), %aggiungo un processo tick
    Data = #{code => Code, length => length(Code), buttons => [], pidTickS => TickServ},
    {ok, locked, Data}. %vai in stato locked (prima nell'enter state) con stato Data (la map)

%config options, state_enter attiva gli enter events 
callback_mode() ->
    [state_functions,state_enter].



%eventi globali
handle_common(info, {_From, Msg}, OldState, _Data) ->
	io:format("ho ricevuto evento da processo!:~n"),
	io:format("mi trovavo in stato: ~p~n", [OldState]),
	case Msg of 
		tick -> 
			tick_received(),
			keep_state_and_data
	end;

handle_common(cast, {down,Button},_OldState, Data) ->
    {keep_state, Data#{button => Button}}; %aggiungi alla map nuovo binding, ossia che bottone premuto è Button

handle_common(cast, {up,Button},_OldState, Data) ->
    case Data of
        #{button := Button} -> %prima è stato chiamato down con questo Button?
            {keep_state, maps:remove(button, Data),
             [{next_event,internal,{button,Button}}]}; %fai scattare controllo codice
        #{} ->
            keep_state_and_data
    end;

%funzione sincrona , richiamabile sempre -> evento globale
handle_common({call,From}, code_length,_OldState, #{code := Code}) ->
    {keep_state_and_data,
     [{reply,From,length(Code)}]}.



%funzione state enter, la prima che parte quando entri nello stato
open(enter, _OldState, _Data) ->
    do_unlock(),
    {cast,
     [{state_timeout,10000,lock}]}; % Time in milliseconds, faccio rimanere aperta sto tempo qua (lock = evento che ricevi allo scattare timeout)

open(state_timeout, lock, Data) ->
    {next_state, locked, Data}; %timer apertura fires, devo chiudere

open(internal, {button,_}, _) ->
	{keep_state_and_data, [postpone]};
?HANDLE_COMMON.

%funzione state enter, la prima che parte quando entri nello stato
locked(enter, _OldState, Data) ->
    do_lock(),
    {keep_state, Data#{buttons := []}};

locked(state_timeout, button, Data) ->
	io:format("Devi essere piu veloce~n", []),
    {keep_state, Data#{buttons := []}};

%controllo che bottoni premuti (sempre tramite chiamata down + up) siano = al codice, se si apri
locked(
  internal, {button,Button},
  #{code := Code, length := Length, buttons := Buttons} = Data) ->
	io:format("hai premuto un pulsante~n", []),
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
  
do_lock() ->
    io:format("Locked~n", []).

do_unlock() ->
    io:format("Open~n", []).

tick_received() ->
	io:format("Ricevuto tick!~n", []).

terminate(_Reason, State, _Data) ->
    State =/= locked andalso do_lock(),
    ok.
  