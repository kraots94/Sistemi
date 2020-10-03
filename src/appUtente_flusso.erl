-module(appUtente_flusso).
-compile(export_all).
-behaviour(gen_statem).
-define(HANDLE_COMMON,
    ?FUNCTION_NAME(T, C, D) -> handle_common(T, C,?FUNCTION_NAME, D)).
-include("globals.hrl").

callback_mode() ->
    [state_functions].

start(TaxiPid) ->
	{ok, Pid} = gen_statem:start_link(?MODULE, TaxiPid, []),
	Pid.

init(InitialPos) ->
	%tick_server:start_clock(?TICKTIME, [self()]),
	{ok, idle, InitialPos}.

%Request = {"a", "b"}.
sendRequest (TaxiPid, UserPid, Request) ->
	%Richiesta = {self(), "f", "d"},}
	gen_statem:cast(UserPid, {send_request,Request,TaxiPid}).


idle(cast, {send_request, Request, PidTaxi}, Position) ->
	checkValidityRequest(Position, Request),
	{From, To} = Request,
	CorrectReq = {self(), From, To},
	macchina_moving_withRecords:updateQueuePid_alone(PidTaxi, CorrectReq),
	keep_state_and_data;

idle(cast, taxiServingYou, Position) ->
	my_util:println("taxi mi sta servendo USER: ", self()),
	{next_state, waiting_car, Position}.

waitin_car_queue(cast, {changeDest, _NewDest}, _Position) ->
	%invio evento non puoi cambiare path...
	keep_state_and_data.

waiting_car(cast, arrivedUserPosition, Position) ->
	my_util:println("taxi arrivato da me USER: ", self()),
	{next_state, moving, Position};

waiting_car(cast, {changeDest, _NewDest}, _Position) ->
	%invio evento cambio dest...
	keep_state_and_data.

moving(cast, arrivedTargetPosition, Position) ->
	my_util:println("sono arrivato a destinazione USER: ", self()),
	my_util:println("ora vado a morire USER: ", self()),
	{next_state, ending, Position};

moving(cast, {changeDest, _NewDest}, _Position) ->
	%invio evento cambio dest...
	keep_state_and_data.

ending(enter, _OldState, _Position) ->
	my_util:println("...Morto..."),
	keep_state_and_data.

%controllo validità richiesta e in caso negativo errore
checkValidityRequest(UserPos, Request) ->
	{From, To} = Request,
	if From /= UserPos -> 
		   exit(wrongPosReq);
	   true ->
		   if From =:= To ->
				  exit(wrongDest);
			  true ->
				  allRight
		   end
	end.

%testing
%make:all(),
%f(),
%PidTaxi = macchina_moving_withRecords:start("f"),
%PidUtente = appUtente_flusso:start("h"),
%appUtente_flusso:sendRequest(PidTaxi,PidUtente ,{"h", "a"}).

