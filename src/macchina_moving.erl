-module(macchina_moving).
-behaviour(gen_statem).
-include("records.hrl").
-include("globals.hrl").
-import('utilities', [println/1, println/2, 
						print_debug_message/1, 
						print_debug_message/2, 
						print_debug_message/3, 
						print_message_raw/1,
						print_car_message/1,
						print_car_message/2,
						print_car_message/3]).
-define(DEBUGPRINT_MOVING, false). %flag che indica abilitazione printing dei messsaggi di Debug
-define(TICKS_TO_CHARGE, 3). %numero di tick da consumare per caricare la batteria 
-define(TICKS_TO_MOVING, 1). %numero di tick da consumare per effettuare uno spostamento
-export([start/1, crash/1, areYouStationary/1, areYouDoingSolarCharge/1, 
		getBatteryLevel/1, getPosition/1, getDataElection/1, getTimeToUser/1,
		enablePathCharge/1, activateSolarCharge/1, updateQueue/3, areYouKillable/1,
		fullBattery/1]).
-export([callback_mode/0, init/1, idle/3, solarCharging/3, 
		moving/3, movingToCharge/3, charging/3, crash/3]).

callback_mode() -> [state_functions,state_enter].

-record(movingCarState, {
				tick_counter,    	   %contatore dei tick generico
				tick_counterBat, 	   %contatore dei tick per la ricarica del mezzo
				pidListener,     	   %pid dell'automa  ascoltatore associato
				nameListener,    	   %nome di questo mezzo
				tappe, 			 	   %lista di record di tipo 'tappa', rappresenta le tappe che questo mezzo deve percorrere
				pathCol, 		 	   %lista di record di tipo 'tappa', rappresenta le tappe per raggiungere una colonnina che questo mezzo potrebbe dover percorrere
				currentUser = none,    %utente corrente 
				currentPos, 	 	   %posizione attuale del mezzo
				batteryLevel, 	 	   %batteria del mezzo
				mustCharge,            %flag che dice se devo o meno andare a caricare (seguire il 'pathCol'), settato da automa batteria
				lastDestination, 	   %nodo dell'ultima destinazione di questo mezzo
				costToLastDestination, %tempo per arrivare al target, decrementa a ogni tick
				timeToUser}). 		   %tempro rimanente per raggiungere il prossimo utente

%% ====================================================================
%% API functions
%% ====================================================================


%costruttore
%@param InitData = {PosizioneIniziale :: string, PidListener :: Pid, Nome:: String}
start(InitData) ->
	{InitialPos, PidListener, Name} = InitData,
	State = #movingCarState {
					tick_counter = 0,
					tick_counterBat = 0,
					pidListener = PidListener,
					nameListener = Name,
					tappe = [],
					pathCol = [],
					currentUser = none,
					currentPos = InitialPos,
					batteryLevel = ?BATTERY_LEVEL_MAX,
					mustCharge = false,
					lastDestination = InitialPos,
					costToLastDestination = 0,
					timeToUser = 0},
	{ok, Pid} = gen_statem:start_link(?MODULE,State, []),
	Pid.

%incidente del veicolo
%usato da @automata macchina_ascoltatore
crash(Pid) ->
	gen_statem:cast(Pid, {crash}).

%----------------------GETTERS METHOS---------------------------------------%
%segue una lunga serie di metodi getters usati da altri automi interni, per i quali definirò un contratto più compatto
%@return true if mezzo è fermo, false altrimenti
%usato da @automata macchina_batteria per i fini della ricarica solare
areYouStationary(Pid) ->
	gen_statem:call(Pid, {areYouStationary}).

%@return true se mezzo è in ricarica solare, false altrimenti
%usato da @automata macchina_batteria 
areYouDoingSolarCharge(Pid) ->
	gen_statem:call(Pid, {doingSolarCharge}).

%@return livello della batteria del mezzo
%usato da @automata macchina_batteria
getBatteryLevel(Pid) ->
	gen_statem:call(Pid, {getBattery}).

%@return posizione attuale del mezzo
%usato da @automata elezione
getPosition(Pid) ->
	gen_statem:call(Pid, {getPosition}).

%@return livello della batteria del mezzo
%usato da @automata elezione per ottenere i dati necessari a trovare il vincitore (batteria, costo al prossimo target, etc.)
getDataElection(Pid) ->
	gen_statem:call(Pid, {getDataElection}).

%@return tempo per raggiungere il target
%usato da @automata ascoltatore per notificare utente del tempo rimanente per essere servito
getTimeToUser(Pid) ->
	gen_statem:call(Pid, {getTimeToUser}).

%----------------------GETTERS METHOS END---------------------------------------%

%abilitazione path verso colonnina ricarica
%usato da @automata batteria 
enablePathCharge(Pid) ->
	gen_statem:cast(Pid, {goToCharge}).

%abilitazione della ricarica solare
%usato da @automata batteria
activateSolarCharge(Pid) ->
	gen_statem:cast(Pid, {activateSolCharge}).

%aggiornamento queue delle tappe del mezzo
%usato da @automata macchina_ascoltatore dopo aver vinto un elezione oppure dopo aver verificato che un cambio destinazione è fattibile
%@param Queue :: List of record 'tappe', tappe da percorrere
%@param Mode :: atomo 'append' o 'replace', indica se le tappe vanno accodate all'attuale queue oppure rimpiazzate (dipende se è stato fatto cambio dest. oppure no)
updateQueue(Pid, Queue, Mode) ->
	gen_statem:cast(Pid, {updateQueue, Queue, Mode}).

%richiesta per sapere se questo automa è terminabile, ossia eliminabile completamente dal servizio
%usato da @automata macchina_ascoltatore
areYouKillable(Pid) ->
	gen_statem:call(Pid, {areYouKillable}).

%notifica di raggiungimento livello massimo batteria, per smettere di caricare il mezzo
%usato da @automata macchina_batteria
fullBattery(Pid) ->
	gen_statem:cast(Pid, {charged}).


%% ====================================================================
%% Automata Functions
%% ====================================================================

init(State) ->
	{ok, idle, State}.

%% ====================================================================
%% handle_common functions - eventi globali
%% ====================================================================

handle_common({call,From}, {areYouKillable}, OldState, State) ->
	if 
		OldState == idle -> 
			{next_state, idle, State, [{reply,From,true}]};
		true -> 
			{next_state, OldState, State, [{reply,From,false}]}
	end;

%automa batt mi dice di andare a caricare, accodo percorso colonnina
handle_common(cast, {goToCharge}, OldState, State) ->
	if 
		OldState /= moving -> 
			keep_state_and_data;
		true ->
			printDebug(State, "aggiorno tappe aggiungendo patch carica"), %nb: l'aggiunta del path è implicito settando il flag
			TappeColonnina = State#movingCarState.pathCol,
			Path = getPathFromCarStops(TappeColonnina),
			CarName = State#movingCarState.nameListener,
			print_car_message(CarName, "Low battery, enable path to charging: ~p", Path),

			{keep_state, State#movingCarState{mustCharge = true}}
	end;

handle_common({call,From}, {areYouStationary}, OldState, State) ->
	Reply = if 
		OldState == idle -> 
			true;
		true -> 
			false
	end,
	{next_state, OldState, State, [{reply,From,Reply}]};

handle_common({call, From}, {doingSolarCharge}, OldState, State) ->
	Reply = if 
		OldState == solarCharging -> 
			true;
		true -> 
			false
	end,
	{next_state, OldState, State, [{reply,From,Reply}]};

handle_common({call,From}, {getBattery}, OldState, State) ->
	Battery = State#movingCarState.batteryLevel,
	{next_state, OldState, State, [{reply,From,Battery}]};

handle_common({call,From}, {getPosition}, OldState, State) ->
	Position = State#movingCarState.currentPos,
	{next_state, OldState, State, [{reply,From,Position}]};

%chiamato da listener per dire a utente tempo rimanente
handle_common({call,From}, {getTimeToUser}, OldState, State) ->
	TimeToUser = State#movingCarState.timeToUser,
	{next_state, OldState, State, [{reply,From,TimeToUser}]};

handle_common(cast, {crash}, _OldState, State) ->
	{next_state, crash, State#movingCarState{tappe = [], currentUser = none,
											timeToUser = 0, costToLastDestination = 0}};

%la ricezione della queue con elezione fatta
%Queue = {costi,Tappe}
% Mode -> append, replace
handle_common(cast, {updateQueue, Queue, Mode}, _OldState, State) ->
	CarName = State#movingCarState.nameListener,
	printDebug(State,"Data Received:"),
	printDebug(State, Queue),
	{{Cost_1, Cost_2, _Cost_3}, TappeTarget, TappeColumn} = Queue,
	Path = getPathFromCarStops(TappeTarget),
	LastDestination = calculateLastDestination(TappeTarget),
	NewState = if 
		Mode == append ->
			Time_To_User_Pos = State#movingCarState.costToLastDestination + Cost_1,
			CostToLastDest = State#movingCarState.costToLastDestination + Cost_1 + Cost_2,
			NewTappe = State#movingCarState.tappe ++ TappeTarget,
			print_car_message(CarName, "Car Stops for client: ~p | Total Time for client: ~w", [Path, CostToLastDest]),
			State#movingCarState{tappe=NewTappe, 
								pathCol = TappeColumn, 
								lastDestination = LastDestination, 
								costToLastDestination = CostToLastDest, 
								timeToUser = Time_To_User_Pos};
		Mode == replace ->
			Time_To_User_Pos = Cost_1,
			CostToLastDest = Cost_1 + Cost_2,
			NewTappe = TappeTarget,
			%controllo se prima tappa indica che sono già da  utente, in tal caso devo toglierla
			PrimaTappa = hd(NewTappe),
			TipoPrimaTappa = PrimaTappa#tappa.type,
			TempoPrimaTappa = PrimaTappa#tappa.t,
			TappeFinali = 
				if (TipoPrimaTappa == user_start) and (TempoPrimaTappa == 0) -> tl(NewTappe);
					true -> NewTappe
				end,
			print_car_message(CarName, "Car Stops for client: ~p | Total Time for client: ~w", [Path, CostToLastDest]),
			State#movingCarState{tappe = TappeFinali, 
								pathCol = TappeColumn, 
								lastDestination = LastDestination, 
								costToLastDestination = CostToLastDest, 
								timeToUser = Time_To_User_Pos};
		true -> printDebug(State,"Can not update queue, Mode not supported"),
			State
	end,

	{next_state, moving, NewState};
%gestione del tick
handle_common(info, {_From, tick}, OldState, State) ->
	if 
		(OldState == idle) or (OldState == crash) -> 
			keep_state_and_data; %in idle e crash ignora il tick
	  	(OldState == charging) or (OldState == solarCharging) ->					
			ActualTickCounter = State#movingCarState.tick_counterBat,
			NewCounter = ActualTickCounter + 1,
			if 
				(NewCounter >= ?TICKS_TO_CHARGE) -> 
					NewState = State#movingCarState{tick_counterBat = 0},
					{keep_state, NewState, [{next_event,internal,charge}]};
	   			true ->  
					NewState = State#movingCarState{tick_counterBat = NewCounter},
					{keep_state, NewState}
			end;
	   	true -> %se in moving o movingToCharge muoviti, consuma tappe
			ActualTickCounter = State#movingCarState.tick_counter,
			NewCounter = ActualTickCounter + 1,
			if 
				(NewCounter >= ?TICKS_TO_MOVING) ->
					NewState = State#movingCarState{tick_counter = 0},
					{keep_state, NewState, [{next_event,internal,move}]};
				true ->  
					NewState = State#movingCarState{tick_counter = NewCounter},
					{keep_state, NewState}
			end
	 end.
		   

idle(enter, _OldState, State) ->
	printDebug(State, "Sono in idle"),
	printState(State),
	keep_state_and_data;

idle({call,From}, {getDataElection}, State) ->
	Cost_To_last_Target = 0,
	Current_Target = State#movingCarState.currentPos,
	Battery_level = State#movingCarState.batteryLevel,
	Packet = {Cost_To_last_Target, Current_Target, Battery_level ,ok},
	{keep_state, State, [{reply,From,Packet}]};

idle(cast, {activateSolCharge}, State) ->
	Battery_level = State#movingCarState.batteryLevel,
	if 
		(Battery_level < ?BATTERY_LEVEL_MAX) ->
			printDebug(State, "sto andando a fare ricarica solare..."),
			{next_state, solarCharging, State};
		true ->
			printDebug(State, "non mi serve carica solare"),
			keep_state_and_data
	end;

?HANDLE_COMMON.

solarCharging(enter, _OldState, _State) ->
	keep_state_and_data;

solarCharging(internal, charge, State) ->
	printState(State),
	ActualBat = State#movingCarState.batteryLevel,
	NewStateCharged = State#movingCarState{batteryLevel = ActualBat + 1},
	{keep_state, NewStateCharged};

solarCharging(cast, {charged}, State) ->
	printDebug(State, "finito di caricare con solare, sono al massimo"),
	{next_state, idle, State#movingCarState{batteryLevel = ?BATTERY_LEVEL_MAX}};

solarCharging(cast, activateSolCharge, State) -> 
	printDebug(State, "Sto gia ricaricando in modo solare"),
	keep_state_and_data;
	
solarCharging({call,From}, {getDataElection}, State) ->
	Cost_To_last_Target = 0,
	Current_Target =  State#movingCarState.currentPos,
	Battery_level = State#movingCarState.batteryLevel,
	Packet = {Cost_To_last_Target, Current_Target, Battery_level, ok},
	{keep_state, State, [{reply,From,Packet}]};

?HANDLE_COMMON.

moving(enter, _OldState, State) ->
	printDebug(State,"Sono in moving"),
	PrimaTappa = hd(State#movingCarState.tappe),
	FirstUser = PrimaTappa#tappa.user,
	TipoTappa = PrimaTappa#tappa.type,
	TempoTappa = PrimaTappa#tappa.t,
	taxiServingAppId(FirstUser, State),
	ActualState = if 
		(TipoTappa =:= user_start) and (TempoTappa == 0) -> %caso speciale in cui primo utente è nel nodo macchina
			arrivedInUserPosition(FirstUser, State),
			NuoveTappe = tl(State#movingCarState.tappe),
			State#movingCarState{currentUser = FirstUser,tappe = NuoveTappe};
		true -> 
			State#movingCarState{currentUser = FirstUser}
	end,	
	printState(ActualState),
	{keep_state, ActualState};

%qui avviene il vero e proprio consumo delle tappe (escluse quelle verso colonnina)
moving(internal, move, State) ->
	%calcolo subito il decremento batteria e costo al last target...visto che mi son spostato
	NewBattery = State#movingCarState.batteryLevel - 1,
	NewCostToLastDest = State#movingCarState.costToLastDestination - 1,
	%e ora lavoro sulle tappe
	TappeAttuali = State#movingCarState.tappe,
	TappaAttuale = hd(TappeAttuali),
	
	NameListener = State#movingCarState.nameListener,
	DataToPrint = [State#movingCarState.currentPos, TappaAttuale#tappa.node_name],

	Time = TappaAttuale#tappa.t,
	NewTime = Time - 1,
	if 
		NewTime > 0 -> %sono nello spostamento fra due nodi, decremento il tempo
			TappaConSpostamento = TappaAttuale#tappa{t = NewTime},
			NuoveTappe = [TappaConSpostamento] ++ tl(TappeAttuali),
			%io:format("-"),
			% print_debug_message(self(), "Sto raggiungendo: ~w", [TappaAttuale#tappa.node_name]),
			%printState(NuovoStato),
			UpdatedState = State#movingCarState{batteryLevel = NewBattery, 
												costToLastDestination = NewCostToLastDest,
												tappe=NuoveTappe},
			{keep_state, UpdatedState};
		true -> %tempo = 0 , quindi ho raggiunto nodo nuovo
			print_debug_message(NameListener, "Teleporting from [~p] to [~p]", DataToPrint),
			TipoNodoAttuale = TappaAttuale#tappa.type,
			PersonaAttuale = TappaAttuale#tappa.user,
			NewState = calculateNewState(State, TappaAttuale, TappeAttuali),
			if  %controllo se sono arrivato al target oppure se sono arrivato da utente nuovo
				TipoNodoAttuale =:= user_target ->
					print_car_message(NameListener, "Arrived in user target position [~p]", TappaAttuale#tappa.node_name),
					arrivedInTargetPosition(PersonaAttuale, State);
				TipoNodoAttuale =:= user_start ->
					arrivedInUserPosition(PersonaAttuale, State);
				true -> 
					nothing_to_do
			end,
			if 
				tl(TappeAttuali) =:= [] -> %ho finito il servizio
					if (State#movingCarState.mustCharge) -> %devo andare a caricare
							printDebug(State, "Moving to charging"),
							checkColumnHere(NewState, NewBattery); %check se la colonnina è qui e in tal caso vai subito in charging
					    true -> 
							UpdatedState = NewState#movingCarState{batteryLevel = NewBattery, 
																	costToLastDestination = 0},
						   	{next_state, idle, UpdatedState}
					end;
				true -> %ho ancora tappe
					UpdatedState = NewState#movingCarState{batteryLevel = NewBattery, 
															costToLastDestination = NewCostToLastDest},
					printState(UpdatedState),
					{keep_state,UpdatedState}
			end
	end;

moving({call,From}, {getDataElection}, State) ->
	Cost_To_last_Target = State#movingCarState.costToLastDestination,
	Current_Target = State#movingCarState.lastDestination,
	Battery_level = State#movingCarState.batteryLevel,
	Packet = {Cost_To_last_Target, Current_Target, Battery_level, ok},
	{keep_state, State, [{reply,From,Packet}]};

moving(cast, {charged}, State) ->
	printDebug(State, "charged in moving, this is an error"),
	keep_state_and_data;

?HANDLE_COMMON.
	
movingToCharge(enter, _OldState, _State) ->
	keep_state_and_data;

movingToCharge({call,From}, {getDataElection}, State) ->
	Cost_To_last_Target = State#movingCarState.costToLastDestination,
	Current_Target =  State#movingCarState.lastDestination,
	Battery_level = State#movingCarState.batteryLevel,
	Packet = {Cost_To_last_Target, Current_Target, Battery_level, ok},
	{keep_state, State, [{reply,From,Packet}]};

%similar to moving
%consumo delle tappe path verso la colonnina ricarica
movingToCharge(internal, move, State) ->
	NewBattery = State#movingCarState.batteryLevel - 1,
	TappeAttuali = State#movingCarState.pathCol,
	TappaAttuale = hd(TappeAttuali),
	NextHop = TappaAttuale#tappa.node_name,
	Time = TappaAttuale#tappa.t,
	NewTime = Time - 1,
	
	if 
		NewTime > 0 -> %sono nello spostamento fra due nodi, decremento il tempo
			TappaConSpostamento = TappaAttuale#tappa{t = NewTime},
			NuoveTappeCol = [TappaConSpostamento] ++ tl(TappeAttuali),
			%il costToLastDest nel caso di spostamento verso col è il costo nexthop
			{keep_state, State#movingCarState{pathCol = NuoveTappeCol, 
											batteryLevel = NewBattery, 
											costToLastDestination = NewTime, 
											lastDestination = NextHop}}; 
		true -> %tempo = 0 , quindi ho raggiunto nodo nuovo
			printDebug(State, "raggiunto nuovo nodo"),
			NewPos = TappaAttuale#tappa.node_name,
			sendPosToGps(NewPos, State),
			TipoNodoAttuale = TappaAttuale#tappa.type,
			if  %controllo in che tipo di nodo sono arrivato (intermedio o colonnina)
				TipoNodoAttuale =:= column -> %arrivato alla colonnina
					printDebug(State, "colonnina raggiunta!"),
					{next_state, charging, State#movingCarState{pathCol = [], 
																batteryLevel = NewBattery, 
																costToLastDestination = 0, 
																lastDestination = NewPos, 
																currentPos = NewPos}};
				true -> %arrivato in nodo intermedio
					NuoveTappe = tl(TappeAttuali),
					ProssimaTappa = hd(NuoveTappe),
					NodoProssimaTappa = ProssimaTappa#tappa.node_name,
					CostoProssimaTappa = ProssimaTappa#tappa.t,
					{keep_state, State#movingCarState{pathCol = NuoveTappe, 
													batteryLevel = NewBattery, 
													lastDestination = NodoProssimaTappa, 
													costToLastDestination = CostoProssimaTappa, 
													currentPos = NewPos}}
			end
	end;
			
?HANDLE_COMMON.
  
charging(enter, _OldState, State) ->
	ListenerName = State#movingCarState.nameListener,
	BatteryLevel = State#movingCarState.batteryLevel,
	print_car_message(ListenerName, "I'm in charging column position, recharging. Current level: ~w", BatteryLevel),
	keep_state_and_data;

charging(internal,charge, State) ->
	ActualBat = State#movingCarState.batteryLevel,
	NewStateCharged = State#movingCarState{batteryLevel = ActualBat + ?TICKS_TO_CHARGE},
	{keep_state, NewStateCharged};

charging({call,From}, {getDataElection}, State) ->
	Cost_To_last_Target = 0,
	Current_Target =  State#movingCarState.currentPos,
	Battery_level = State#movingCarState.batteryLevel,
	Packet = {Cost_To_last_Target, Current_Target, Battery_level, ok},
	{keep_state, State, [{reply,From,Packet}]};

charging(cast, {charged}, State) ->
	printDebug(State, "finito di caricare, sono al massimo"),
	{next_state, idle, State#movingCarState{batteryLevel = ?BATTERY_LEVEL_MAX, mustCharge = false}};

?HANDLE_COMMON.
  
crash(enter, _OldState, State) ->
	print_debug_message(State#movingCarState.nameListener, "Accident happen!"),
	{keep_state, State#movingCarState{tappe = [],currentUser = none, costToLastDestination = 0, timeToUser = 0}};

crash({call,From}, {getDataElection}, State) ->
	Cost_To_last_Target = 0,
	Current_Target =  none,
	Battery_level = none,
	Packet = {Cost_To_last_Target, Current_Target, Battery_level, not_ok},
	{keep_state, State, [{reply,From,Packet}]};

crash(cast, {fixed}, State) ->
	print_debug_message(State#movingCarState.nameListener, "Fixed Problem!"),
	{next_state, idle, State};

?HANDLE_COMMON.
%% ====================================================================
%% Internal functions
%% ====================================================================

%return cambiamento stato macchina dopo essere arrivato a nuovo nodo e invia relativi eventi a utenti nuovi o raggiunti
calculateNewState(Stato, TappaAttuale, Tappe) ->
	NuoveTappe = tl(Tappe),
	Pos = TappaAttuale#tappa.node_name,
	sendPosToUser(Stato#movingCarState.currentUser,Pos,Stato),
	sendPosToGps(Pos, Stato),
	if 
		NuoveTappe =:= [] -> 
			Stato#movingCarState{tappe = [], 
								currentUser = none,
								currentPos = Pos, 
								lastDestination = Pos};
		true ->
			TipoNodoAttuale = TappaAttuale#tappa.type,
			NewState = if %controllo se sono arrivato da utente
				TipoNodoAttuale =:= user_start -> 
					Stato#movingCarState{timeToUser = 0};
				true -> 
					Stato 
			end,
			ProssimaTappa = hd(NuoveTappe),
			ProssimoUtente = ProssimaTappa#tappa.user,
			ProssimoTipo = ProssimaTappa#tappa.type,
			ProssimoTempo = ProssimaTappa#tappa.t,
			if 
				ProssimoUtente /= TappaAttuale#tappa.user ->
					taxiServingAppId(ProssimoUtente, NewState),
					if 
						(ProssimoTipo =:= user_start) and (ProssimoTempo =:= 0) -> %caso in cui il prossimo utente è nel nodo attuale
							arrivedInUserPosition(ProssimoUtente, NewState),
							NuoveNuoveTappe = tl(NuoveTappe),
							NewState#movingCarState{tappe = NuoveNuoveTappe, 
													currentUser = ProssimoUtente, 
													currentPos = Pos};
					   true ->
							NewState#movingCarState{tappe = NuoveTappe, 
													currentUser = ProssimoUtente, 
													currentPos = Pos}
					end;
				true ->
					NewState#movingCarState{tappe = NuoveTappe, currentPos = Pos}
			end
	end.

%calcolo target finale tra utenti accodati
calculateLastDestination(Tappe) ->
	LastTap = hd(lists:reverse(Tappe)),
	LastTap#tappa.node_name.
	
sendPosToGps(CurrentPosition,S) ->
	ListenerPid = S#movingCarState.pidListener,
	macchina_ascoltatore:updatePosition(ListenerPid, CurrentPosition).
  
sendPosToUser(UserPid, Position, S) ->
	ListenerPid = S#movingCarState.pidListener,
	macchina_ascoltatore:sendToEsternalAutomata(ListenerPid, UserPid, {newNodeReached, Position}).

taxiServingAppId(UserPid, S) ->
	ListenerPid = S#movingCarState.pidListener,
	macchina_ascoltatore:sendToEsternalAutomata(ListenerPid, UserPid, taxiServingYou).

arrivedInUserPosition(UserPid, S) ->
	ListenerPid = S#movingCarState.pidListener,
	gen_statem:cast(ListenerPid, {carrying, UserPid}),
	macchina_ascoltatore:sendToEsternalAutomata(ListenerPid, UserPid, arrivedUserPosition).

arrivedInTargetPosition(UserPid, S) ->
	ListenerPid = S#movingCarState.pidListener,
	gen_statem:cast(ListenerPid, {noMoreCarrying}),
	macchina_ascoltatore:sendToEsternalAutomata(ListenerPid, UserPid, arrivedTargetPosition).

checkColumnHere(State, Battery) ->
	%controllo se la colonnina è qua dove sono
	PrimaTappa = hd(State#movingCarState.pathCol),
	TipoTappa = PrimaTappa#tappa.type,
	TempoTappa = PrimaTappa#tappa.t,
	if 
		(TipoTappa =:= column) and (TempoTappa == 0) -> %caso speciale in cui colonnina è dove sono
			NewState = State#movingCarState{
							pathCol = [], 
							costToLastDestination = 0,
							batteryLevel = Battery
							},
			{next_state, charging, NewState};
		true ->
			printDebug(State, "Sono in moving verso la colonnina"),
			NewState = State#movingCarState{batteryLevel = Battery, 
											costToLastDestination = 0
											},
			{next_state, movingToCharge, NewState}
	end.

printDebug(S, ToPrint) ->
	if ?DEBUGPRINT_MOVING -> utilities:print_debug_message(S#movingCarState.nameListener, [?TILDE_CHAR] ++ "p", ToPrint);
	   true -> foo
	end.

printState(State) ->
	printDebug(State,State).
	
getPathFromCarStops(Tappe) ->
	MapFunc = fun(T) ->
		Node_name = T#tappa.node_name,
		Node_name
	end,
	OutList = lists:map(MapFunc, Tappe),
	OutList.