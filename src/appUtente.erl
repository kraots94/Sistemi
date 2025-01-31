-module(appUtente).
-behaviour(gen_statem).
-export([start/1, sendRequest/2, updatePosition/2, changeDestination/2]).
-export([callback_mode/0, init/1, idle/3, waiting_election/3, waiting_car/3, waiting_car_queued/3, moving/3]).
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
-define(DEBUGPRINT_APP, false).		%flag che indica abilitazione printing dei messsaggi di Debug
-define(TIME_TO_WAIT_REPLY, 3000).  %tempo massimo per aspettare risposte dal servizio dopo aver inviato richiesta,
									%se dopo tale tempo non è stata ricevuta una risposta verrà reiterata una nuova richiesta 

%range di tempo di attesa da aspettare prima di reiterare una richiesta di servizio
-define(MIN_RANDOM_TIME_WAIT, 2000).
-define(MAX_RANDOM_TIME_WAIT, 4000).


-record(appUserState, {
						pidGPSModule,		%pid del modulo GPS associato al dispositivo
						pidCarServing,  	%pid del taxi che sta servendo utente
						pidUser,	    	%pid dell'utente associato a quest'app
						nameCarServing, 	%nome del taxi che sta servendo app
						nameUser,			%il nome dell'utente associato a quest'app
						currentPos,	   		%la posizione corrente dell'utente associato
						currentDestination, %la destinazione corrente dell'utente associato
						request,  			%l'ultima richiesta effettuata dall'utente associato
						taxiIsServingMe     %flag che indica se il taxi sta servendo me (ossia se sono la persona in coda alla sua lista di utenti)
					}).

callback_mode() -> [state_functions, state_enter].
%% ====================================================================
%% API functions
%% ====================================================================

%costruttore
%@param InitData = {PosizioneIniziale :: string, PidGpsServer :: Pid, PidLinkedUser :: Pid}
start(InitData) ->
	{ok, Pid} = gen_statem:start_link(?MODULE,InitData, []),
	Pid.

%Invio della richiesta di spostamento
%usato da @automata utente
%@param App_Pid :: Pid, app a cui richiedere il servizio
%@param Request :: Tuple, contiene richiesta di spostamento, formattata come {From :: String, To :: String}
%@return atomo 'from_pos_not_user_pos' se richiesta errata ossia partenza non combacia con posizione utente
%			   'from_and_dest_same' se richiesta errata ossia la partenza combacia con l'arrivo
%			   'valid' richiesta corretta
sendRequest (App_Pid, Request) ->
	UserPos = gen_statem:call(App_Pid, {getPos}),
	Out = checkValidityRequest(UserPos, Request),
	if Out == valid -> 
			gen_statem:cast(App_Pid, {send_request,Request}),
			ok;
		true ->
			Out
	end.

%aggiornamento della posizione dell'utente dopo lo spostamento della macchina che lo sta servendo
%usato da @automata macchina_ascoltatore
%@param App_Pid :: Pid, app alla quale aggiornare la posizione 
%@param NewNode :: String, Nuovo nodo raggiunto
updatePosition(App_Pid, NewNode) ->
	gen_statem:cast(App_Pid, {newNodeReached, NewNode}).

%cambiamento destinazione 
%usato da @automata utente
%@param App_Pid :: Pid, app alla quale inviare richiesta del cambio di destinazione
%@param NewTarget :: String, nuovo nodo destinazione richiesto
%@return atomo 'user_in_car_queue' se impossibile cambiare destinazione per utente in coda,
%		 atomo 'not_enough_battery' se richiesta non è gestibile dalla macchina che serve l'utente
%		 atomo 'changed_path' se la richiesta è stata accettata e presa in carico
changeDestination(App_Pid, NewTarget) ->
	gen_statem:call(App_Pid, {changeDest, NewTarget}).

%% ====================================================================
%% Automata Functions
%% ====================================================================
init(InitData) ->
	{InitialPos, PidGpsServer, PidUser, Name} = InitData,
	PidGpsModule = gps_module:start_gps_module(
					initDataGpsModule(PidGpsServer,InitialPos, Name)), %start gps module (and register)
	State = #appUserState{
		pidGPSModule = PidGpsModule, 
		pidUser = PidUser,
		nameUser = Name,
		currentPos = InitialPos,
		currentDestination = none,
		request = {},
		pidCarServing = none,
		nameCarServing = "",
		taxiIsServingMe = false
	}, 
	{ok, idle, State}.

%% ====================================================================
%% handle_common functions - eventi globali
%% ====================================================================

%ricezione del cambiamento posizione dall'auto che mi serve
handle_common(cast, {newNodeReached, NewNode}, OldState, State) ->
	NewState = if 
		OldState == moving -> %ricevuto mentre sono in moving, la mia posizione cambia!
			sendPosToGps(NewNode,State),
		   	State#appUserState{currentPos = NewNode};
		true -> %ricevuto non in moving, potrei dire a utente servito dov'è auto mentre si sposta da lui
			State
	end,
	{keep_state, NewState};

%ricezione del comando di terminazione
handle_common(cast, {die}, _OldState, State) ->
	PidGpsModule = State#appUserState.pidGPSModule,
	gps_module:end_gps_module(PidGpsModule),
	gen_statem:stop(self());

%richiesta di ottenere posizione attuale 
handle_common({call,From},{getPos}, OldState, State) ->
	CurrentPos = State#appUserState.currentPos,
	{next_state, OldState, State, [{reply,From,CurrentPos}]};
  		   
%ricezione del cambio destinazione
handle_common({call,From}, {changeDest, NewTarget}, OldState, State) ->
	{_Start, To} = State#appUserState.request,
	CurrentPosition = State#appUserState.currentPos,
	printDebugList([From]),
	{Reply, NewState} = if 
		(NewTarget /= To) and (NewTarget /= CurrentPosition) ->
			if 
				(OldState == waiting_car) or (OldState == moving) ->
					PidCar = State#appUserState.pidCarServing,
					Res = macchina_ascoltatore:changeDestination(PidCar, {CurrentPosition, NewTarget, self()}),
					if  Res == changed_path ->
							{changed_path, State#appUserState{request = {CurrentPosition, NewTarget}}};
						true -> 
							{Res, State}
					end;
				true ->
				    {user_in_car_queue, State}
			end;
		true ->
			{invalid_target, State}
	end,
	{next_state, OldState, NewState, [{reply, From, Reply}]}.		
		
idle(enter, _OldState, _Stato) -> keep_state_and_data;
 
idle(internal,{send_request, Request}, State) ->
	sendRequestToTaxi(Request,State);

idle(cast, {send_request, Request}, State) ->
	sendRequestToTaxi(Request,State);

?HANDLE_COMMON.

waiting_election(enter, _OldState, _Stato) -> 
	%richiesta inviata, aspetto risultati
	keep_state_and_data;

%non ho ricevuto risposta dentro al timer
waiting_election(state_timeout, noReply, Stato) ->
	print_user_message(Stato#appUserState.nameUser, "App_User - no reply from taxi, going back to begin election"),
 	Request = Stato#appUserState.request, %prendo la request
	{next_state, idle, Stato, [{next_event,internal,{send_request,Request}}]};
	
%ottenimento dati dall'elezione, inviati da ascoltatore taxi
waiting_election(cast, {winner, Data}, Stato) -> 
	IdCarWinner = Data#election_result_to_user.id_car_winner,
	if IdCarWinner == -1 -> %non c'è un vincitore, devo riprovare fra un po' di tempo
			print_user_message(Stato#appUserState.nameUser, "App_User - No taxi has won election running election."), 
			Request = Stato#appUserState.request, %prendo la request
			wait_random_time(),
			print_user_message(Stato#appUserState.nameUser, "App_User - Waited random time, going back to begin election"),
			{next_state, idle, Stato, [{next_event,internal,{send_request,Request}}]};
	true -> %hooray vincitore trovato
			NameCar = Data#election_result_to_user.name_car,
			sendToUser({gotElectionData,Data},Stato),
			{next_state, waiting_car_queued, Stato#appUserState{pidCarServing = IdCarWinner,
														nameCarServing = NameCar}}
	end;

%auto a cui ho inviato richiesta è occupata, sta eseguendo altra elezione 
waiting_election(cast, {already_running_election_wait}, Stato) -> 
	print_user_message(Stato#appUserState.nameUser, "App_User - Nearest Taxi is already running election."), 
	Request = Stato#appUserState.request, %prendo la request
	wait_random_time(), %aspetto un tempo random 
	print_user_message(Stato#appUserState.nameUser, "App_User - Waited random time, going back to begin election"),
	{next_state, idle, Stato, [{next_event,internal,{send_request,Request}}]}.


waiting_car_queued(enter, _OldState, _Stato) -> keep_state_and_data;

waiting_car_queued(cast, {changeDest, _NewDest}, _State) ->
	%invio evento non puoi cambiare path...
	keep_state_and_data;

waiting_car_queued(cast, taxiServingYou, State) ->
	{next_state, waiting_car, State};

waiting_car_queued(cast, {crash}, Stato) ->
	CurrentPos = Stato#appUserState.currentPos,
	{_From, To} = Stato#appUserState.request,
	Request = {CurrentPos,To},
	UserPid = Stato#appUserState.pidUser,
	gen_statem:cast(UserPid, {crash}),
	print_user_message(Stato#appUserState.nameUser, "App_User - Car Crashed"),
	wait_random_time(),
	print_user_message(Stato#appUserState.nameUser, "App_User - Waited random time after car crashed, going back to begin election"),
	{next_state, idle, Stato, [{next_event,internal,{send_request,Request}}]};

?HANDLE_COMMON.

waiting_car(enter, _OldState, _Stato) -> keep_state_and_data;

waiting_car(cast, {crash}, Stato) ->
	CurrentPos = Stato#appUserState.currentPos,
	{_From, To} = Stato#appUserState.request,
	Request = {CurrentPos,To},
	UserPid = Stato#appUserState.pidUser,
	gen_statem:cast(UserPid, {crash}),
	print_user_message(Stato#appUserState.nameUser, "App_User - Car Crashed"),
	wait_random_time(),
	print_user_message(Stato#appUserState.nameUser, "App_User - Waited random time after car crashed, going back to begin election"),
	{next_state, idle, Stato, [{next_event,internal,{send_request,Request}}]};

waiting_car(cast, taxiServingYou, State) ->
	{keep_state, State#appUserState{taxiIsServingMe = true}};

waiting_car(cast, arrivedUserPosition, State) ->
	sendToUser({arrivedUserPosition, State#appUserState.nameCarServing}, State),
	{next_state, moving, State};
	
?HANDLE_COMMON.


moving(enter, _OldState, _Stato) -> keep_state_and_data;

moving(cast, {crash}, Stato) ->
	CurrentPos = Stato#appUserState.currentPos,
	{_From, To} = Stato#appUserState.request,
	Request = {CurrentPos,To},
	UserPid = Stato#appUserState.pidUser,
	gen_statem:cast(UserPid, {crash}),
	print_user_message(Stato#appUserState.nameUser, "App_User - Car Crashed"),
	wait_random_time(),
	print_user_message(Stato#appUserState.nameUser, "App_User - Waited random time after car crashed, going back to begin election"),
	{next_state, idle, Stato, [{next_event,internal,{send_request,Request}}]};

moving(cast, arrivedTargetPosition, State) ->
	sendToUser({arrivedTargetPosition, State#appUserState.currentPos}, State),
	{next_state, idle, State};
	

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

%controllo validità richiesta e in caso negativo torna atomo appropriato
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

%trova il taxi piu' vicino, se non presente torna 'none'
findTaxi(State) ->
	send_message(State#appUserState.pidGPSModule, {getNearestCar}),
	receive 
		{already_running_election_wait} -> findTaxi(State); %potresti riceverli 
		Nearest -> Nearest
	end.

sendRequestToTaxi(Request, State) ->
	printDebug("Request"),
	{From, To} = Request,
	CorrectRequest = {From, To, self()},
	NearestCar = findTaxi(State),
	if NearestCar == none ->  %nessuna macchina nei paraggi
		   	wait_random_time(),
			print_user_message(State#appUserState.nameUser, "App_User - Waited random time, no one near, going back to begin election"),
			{next_state, idle, State, [{next_event,internal,{send_request,Request}}]};
	   true ->
		   gen_statem:cast(NearestCar, {beginElection, CorrectRequest}),
		   {next_state, waiting_election, State#appUserState{request = Request}, [{state_timeout,?TIME_TO_WAIT_REPLY,noReply}]}
	end.
	

initDataGpsModule(PidGpsServer, InitialPosition, Name) ->
	#dataInitGPSModule{
		pid_entity = self(), 
		type = user, 
		pid_server_gps = PidGpsServer,
		starting_pos = InitialPosition, 
		signal_power = ?GPS_MODULE_POWER, 
		map_side = ?MAP_SIDE,
		name_entity = Name}.

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
