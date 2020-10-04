-module(appUtente_flusso).
-compile(export_all).
-behaviour(gen_statem).
-include("globals.hrl").
-include("records.hrl").

callback_mode() -> [state_functions].
%userState
%                   pidEnv,
%					pidWirelessCard,
%					currentPos,
%					currentDestination
%% ====================================================================
%% API functions
%% ====================================================================

start(InitialPos, PidEnv, PidWireless) ->
	State = #userState{pidEnv = PidEnv, pidWirelessCard = PidWireless, currentPos = InitialPos},
	{ok, Pid} = gen_statem:start_link(?MODULE,State, []),
	sendPosToGps(PidWireless,InitialPos),
	Pid.

%Request è tipo = {"a", "b"}.
sendRequest (UserPid, Request) ->
	%Richiesta = {self(), "f", "d"},}
	gen_statem:cast(UserPid, {send_request,Request}).

updatePosition(UserPid, NewNode) ->
	gen_statem:cast(UserPid, {newNodeReached, NewNode}).

%% ====================================================================
%% Automata Functions
%% ====================================================================

init(State) ->
	%tick_server:start_clock(?TICKTIME, [self()]),
	{ok, idle, State}.

%ricezione del cambiamento posizione auto che mi serve
handle_common(cast, {newNodeReached, NewNode}, OldState, State) ->
	NewState = if OldState == moving -> %ricevuto mentre sono in moving, la mia posizione cambia!
					sendPosToGps(State#userState.pidWirelessCard, NewNode),
		   			State#userState{currentPos = NewNode};
				  true -> %ricevuto non in moving, potrei dire a utente dov'è auto se son servito
					State
			   end,
	{keep_state, NewState}.
  		   
	

idle(cast, {send_request, Request}, State) ->
	checkValidityRequest(State#userState.currentPos, Request),
	{From, To} = Request,
	CorrectReq = {self(), From, To},
	PidWinnerTaxi = findTaxi(State),
	macchina_moving_withRecords:updateQueuePid_alone(PidWinnerTaxi, CorrectReq),
	keep_state_and_data;

idle(cast, taxiServingYou, State) ->
	my_util:println("taxi mi sta servendo USER: ", self()),
	{next_state, waiting_car, State}.

%waitin_car_queue(cast, {changeDest, _NewDest}, _State) ->
%	%invio evento non puoi cambiare path...
%	keep_state_and_data.

waiting_car(cast, arrivedUserPosition, State) ->
	my_util:println("taxi arrivato da me USER: ", self()),
	{next_state, moving, State};
	
?HANDLE_COMMON.

%waiting_car(cast, {changeDest, _NewDest}, _Position) ->
%	%invio evento cambio dest...
%	keep_state_and_data.

moving(cast, arrivedTargetPosition, State) ->
	my_util:println("sono arrivato a destinazione USER: ", self()),
	my_util:println("ora vado a morire USER: ", self()),
	deletePos(State#userState.pidWirelessCard),
	{next_state, ending, State};

%moving(cast, {changeDest, _NewDest}, _State) ->
%	%invio evento cambio dest...
%	keep_state_and_data;
	

?HANDLE_COMMON.
  
ending(enter, _OldState, _State) ->
	my_util:println("...Morto..."),
	keep_state_and_data.

%% ====================================================================
%% Internal Functions
%% ====================================================================

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

findTaxi(State) ->
	WirelessCardPid = State#userState.pidWirelessCard,
	MyPos = State#userState.currentPos,
	WirelessCardPid ! {self(), {getNearEntities, MyPos, 50}},
	receive 
				NearEntities -> hd(NearEntities)
	end.
	
sendPosToGps(WirelessCardPid, Position) ->
	my_util:println("SONO UTENTE INVIO POS"),
	WirelessCardPid ! {self(), {setPosition, Position}}.
  
deletePos(WirelessCardPid) ->
	%wirelessCardPid ! {self(), {removePosition}}.
	ok.

%testing
%make:all(),
%f(),
%PidTaxi = macchina_moving_withRecords:start("f"),
%PidUtente = appUtente_flusso:start("h"),
%appUtente_flusso:sendRequest(PidTaxi,PidUtente ,{"h", "a"}).


%make:all(),
%f(),
%PidTaxi = macchina_moving_withRecords:start("a", 0),
%PidUser = appUtente_flusso:start("b", 0, 0),
%appUtente_flusso:sendRequest(PidUser, {"b","i", PidTaxi}).

