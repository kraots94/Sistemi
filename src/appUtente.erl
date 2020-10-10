-module(appUtente).
-compile(export_all).
-behaviour(gen_statem).
-import('send', [send_message/2, send_message/3]).
-import('utilities', [println/1, println/2, 
						print_debug_message/1, 
						print_debug_message/2, 
						print_debug_message/3,
						print_user_message/1,
						print_user_message/2,
						print_user_message/3,
						generate_random_number/2]).
-include("globals.hrl").
-include("records.hrl").
-define(DEBUGPRINT_APP, false).

% time is in milliseconds
-define(MIN_RANDOM_TIME_WAIT, ?TICKTIME * 2 * 1000).
-define(MAX_RANDOM_TIME_WAIT, ?TICKTIME * 4 * 1000).

callback_mode() -> [state_functions, state_enter].
-record(appUserState, {
						pidGPSModule,
						pidCarServing,
						pidUser,
						currentPos,
						currentDestination,
						request,
						taxiIsServingMe
					}).
%% ====================================================================
%% API functions
%% ====================================================================

%InitData = {InitialPos, PidGpsServer, PidUser}
start(InitData) ->
	{ok, Pid} = gen_statem:start_link(?MODULE,InitData, []),
	Pid.

%Request è tipo = {"a", "b"}.
sendRequest (App_Pid, Request) ->
	UserPos = gen_statem:call(App_Pid, {getPos}),
	Out = checkValidityRequest(UserPos, Request),
	if Out == valid -> 
			gen_statem:cast(App_Pid, {send_request,Request}),
			ok;
		true ->
			Out
	end.

updatePosition(UserPid, NewNode) ->
	gen_statem:cast(UserPid, {newNodeReached, NewNode}).

%send to nearest req without election (debugging)
sendRequestNoElection(UserPid, Request) ->
	gen_statem:cast(UserPid, {send_requestNoElection, Request}).

%% ====================================================================
%% Automata Functions
%% ====================================================================
init(InitData) ->
	{InitialPos, PidGpsServer, PidUser} = InitData,
	PidGpsModule = gps_module:start_gps_module(
					initDataGpsModule(PidGpsServer,InitialPos)), %start gps module (and register)
	State = #appUserState{
		pidGPSModule = PidGpsModule, 
		pidUser = PidUser,
		currentPos = InitialPos,
		currentDestination = none,
		request = none,
		pidCarServing = none,
		taxiIsServingMe = false
	}, 
	{ok, idle, State}.

%ricezione del cambiamento posizione auto che mi serve
handle_common(cast, {newNodeReached, NewNode}, OldState, State) ->
	NewState = if 
		OldState == moving -> %ricevuto mentre sono in moving, la mia posizione cambia!
			sendPosToGps(NewNode,State),
		   	State#appUserState{currentPos = NewNode};
		true -> %ricevuto non in moving, potrei dire a utente servito dov'è auto mentre si sposta da lui
			State
	end,
	{keep_state, NewState};

handle_common(cast, {die}, _OldState, State) ->
	PidGpsModule = State#appUserState.pidGPSModule,
	gps_module:end_gps_module(PidGpsModule),
	gen_statem:stop(self());

handle_common({call,From},{getPos}, OldState, State) ->
	CurrentPos = State#appUserState.currentPos,
	{next_state, OldState, State, [{reply,From,CurrentPos}]}.
  		   
idle(enter, _OldState, _Stato) -> keep_state_and_data;
 
idle(internal,{send_request, Request}, State) ->
	sendRequestToTaxi(Request,State);

idle(cast, {send_request, Request}, State) ->
	sendRequestToTaxi(Request,State);

?HANDLE_COMMON.

waiting_election(enter, _OldState, _Stato) -> 
	%richiesta inviata, aspetto risultati
	keep_state_and_data;

waiting_election(cast, {winner, Data}, Stato) -> 
	IdCarWinner = Data#election_result_to_user.id_car_winner,
	if IdCarWinner == -1 -> %non c'è un vincitore, devo riprovare fra un po' di tempo
			print_user_message(Stato#appUserState.pidUser, "App_User - No taxi has won election running election."), 
			Request = Stato#appUserState.request, %prendo la request
			wait_random_time(),
			print_user_message(Stato#appUserState.pidUser, "App_User - Waited random time, going back to begin election"),
			{next_state, idle, Stato, [{next_event,internal,{send_request,Request}}]};
	true -> %hooray vincitore trovato
			sendToUser({gotElectionData,Data},Stato),
			{next_state, waiting_car, Stato#appUserState{pidCarServing = IdCarWinner}}
	end;

waiting_election(cast, {already_running_election_wait}, Stato) -> 
	print_user_message(Stato#appUserState.pidUser, "App_User - Nearest Taxi is already running election."), 
	Request = Stato#appUserState.request, %prendo la request
	wait_random_time(), %aspetto un tempo random 
	print_user_message(Stato#appUserState.pidUser, "App_User - Waited random time, going back to begin election"),
	{next_state, idle, Stato, [{next_event,internal,{send_request,Request}}]}.

%waitin_car_queue(cast, {changeDest, _NewDest}, _State) ->
%	%invio evento non puoi cambiare path...
%	keep_state_and_data.

waiting_car(enter, _OldState, _Stato) -> keep_state_and_data;

waiting_car(cast, taxiServingYou, State) ->
	{keep_state, State#appUserState{taxiIsServingMe = true}};

waiting_car(cast, arrivedUserPosition, State) ->
	sendToUser({arrivedUserPosition, State#appUserState.pidCarServing}, State),
	{next_state, moving, State};
	
?HANDLE_COMMON.

%waiting_car(cast, {changeDest, _NewDest}, _Position) ->
%	%invio evento cambio dest...
%	keep_state_and_data.

moving(enter, _OldState, _Stato) -> keep_state_and_data;

moving(cast, arrivedTargetPosition, State) ->
	sendToUser({arrivedTargetPosition, State#appUserState.currentPos}, State),
	{next_state, idle, State};

%moving(cast, {changeDest, _NewDest}, _State) ->
%	%invio evento cambio dest...
%	keep_state_and_data;
	

?HANDLE_COMMON.

%% ====================================================================
%% Internal Functions
%% ====================================================================
sendPosToGps(CurrentPosition,S) ->
	GpsModulePid = S#appUserState.pidGPSModule,
	gps_module:setPosition(GpsModulePid, CurrentPosition).

sendToUser(Data, S) ->
	PidUser = S#appUserState.pidUser,
	utente:receiveFromApp(PidUser, Data).

deletePosGps(S) ->
	gps_module:deleteLocationTracks(S#appUserState.pidGPSModule).

%controllo validità richiesta e in caso negativo errore
checkValidityRequest(UserPos, Request) ->
	{From, To} = Request,
	if From /= UserPos -> 
			from_pos_not_user_pos;
		true ->
			if 
				From =:= To ->
					from_and_dest_same;
				true ->
					valid
			end
	end.

%trova il taxi piu' vicino (in futuro basic elezione)
findTaxi(State) ->
	send_message(State#appUserState.pidGPSModule, {getNearestCar}),
	receive 
		Nearest -> Nearest
	end.

sendRequestToTaxi(Request, State) ->
	NearestCar = findTaxi(State),
	{From, To} = Request,
	CorrectRequest = {From, To, self()},
	gen_statem:cast(NearestCar, {beginElection, CorrectRequest}),
	{next_state, waiting_election, State#appUserState{request = Request}}.
	

initDataGpsModule(PidGpsServer, InitialPosition) ->
	#dataInitGPSModule{
		pid_entity = self(), 
		type = user, 
		pid_server_gps = PidGpsServer,
		starting_pos = InitialPosition, 
		signal_power = ?GPS_MODULE_POWER, 
		map_side = ?MAP_SIDE}.

printDebug(ToPrint) ->
	if ?DEBUGPRINT_APP -> print_debug_message(self(), [?TILDE_CHAR] ++ "p", ToPrint);
	   true -> not_printed
	end.

printDebugList(ToPrint) ->
	if ?DEBUGPRINT_APP -> print_debug_message(self(), [?TILDE_CHAR] ++ "w", ToPrint);
	   true -> not_printed
	end.

wait_random_time() ->
	Random_Time = generate_random_number(?MIN_RANDOM_TIME_WAIT, ?MAX_RANDOM_TIME_WAIT),
	timer:sleep(Random_Time).
