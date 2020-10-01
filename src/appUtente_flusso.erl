-module(appUtente_flusso).
-compile(export_all).
-behaviour(gen_statem).
-define(HANDLE_COMMON,
    ?FUNCTION_NAME(T, C, D) -> handle_common(T, C,?FUNCTION_NAME, D)).

-record(tappa, {user,
				tipo = "",
				tempo = 0,
				posizione = {0,0}
				}).

callback_mode() ->
    [state_functions].

start(TaxiPid) ->
	{ok, Pid} = gen_statem:start_link(?MODULE, TaxiPid, []),
	Pid.

init(TaxiPid) ->
	%tick_server:start_clock(?TICKTIME, [self()]),
	sendQueue1(TaxiPid),
	{ok, waiting_car_queue, TaxiPid}.

sendQueue1(TaxiPid) ->
	Record1 = #tappa {user = self(), tipo = "normale", tempo = 3, posizione = {1,1}},
	Record2 = #tappa {user = self(), tipo = "partenza", tempo = 5, posizione = {2,2}},
	Record3 = #tappa {user = self(), tipo = "normale", tempo = 3, posizione = {3,3}},
	Record4 = #tappa {user = self(), tipo = "arrivo", tempo = 4, posizione = {4,4}},
	Queue = [Record1, Record2, Record3, Record4],
	macchina_moving_withRecords:updateQueuePid_alone(TaxiPid, Queue).

%idle(enter, _OldState, Stato) ->
%	my_util:println("<Utente> sono in idle..."),
%	sendQueue1(Stato),
%	keep_state_and_data;

waiting_car_queue(cast, taxiServingYou, Taxi) ->
	my_util:println("taxi mi sta servendo USER: ", self()),
	{next_state, waiting_car, Taxi}.

waitin_car_queue(cast, {changeDest, _NewDest}, _Taxi) ->
	%invio evento non puoi cambiare path...
	keep_state_and_data.

waiting_car(cast, arrivedUserPosition, Taxi) ->
	my_util:println("taxi arrivato da me USER: ", self()),
	{next_state, moving, Taxi};

waiting_car(cast, {changeDest, _NewDest}, _Taxi) ->
	%invio evento cambio dest...
	keep_state_and_data.

moving(cast, arrivedTargetPosition, Taxi) ->
	my_util:println("sono arrivato a destinazione USER: ", self()),
	{next_state, ending, Taxi};

moving(cast, {changeDest, _NewDest}, _Taxi) ->
	%invio evento cambio dest...
	keep_state_and_data.

ending(enter, _OldState, _Taxi) ->
	my_util:println("Sto per morire harry..."),
	keep_state_and_data.

%testing
%c(macchina_moving_withRecords),
%c(appUtente_flusso),
%f(),
%PidTaxi = macchina_moving_withRecords:start({44,33}),
%PidUtente = appUtente_flusso:start(PidTaxi).

