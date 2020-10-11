-module(macchina_moving).
-compile(export_all).
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
-define(DEBUGPRINT_MOVING, false).
-define(TICKS_TO_CHARGE, 3).
-define(TICKS_TO_MOVING, 1).


callback_mode() -> [state_functions,state_enter].
%getDataElection, deve tornare {Cost_To_last_Target, Current_Target, Battery_Level} , Current_Target = pos attuale se fermo, dest se moving, next node se vai verso colonnina (stringa)
%diminuizione batteria a ogni tick, e anche cost to last target.
%Current_Target = next node se stai andando verso colonnina

%stato macchina in movimento:
-record(movingCarState, {
				tick_counter,
				tick_counterBat,
				pidListener,
				nameListener,
				tappe, %lista di tappe da percorrere
				pathCol, %lista di tappe verso la colonnina
				currentUser = none,
				currentPos,
				batteryLevel, %decrementa a ogni tick
				mustCharge, %flag che dice se devo o meno andare a caricare (seguire tappe colonnina)
				lastDestination, %nodo dell'ultimo target
				costToLastDestination, %tempo per arrivare al target, decrementa a ogni tick
				timeToUser}). 

%% ====================================================================
%% API functions
%% ====================================================================

%InitData = {InitialPos, PidListener, Name}
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

crash(Pid) ->
	gen_statem:cast(Pid, {crash}).

%NB: questo torna la risposta al chiamante, se imposti timer e non c'è risposta invece la chiamata fallisce
areYouStationary(Pid) ->
	gen_statem:call(Pid, {areYouStationary}).

areYouDoingSolarCharge(Pid) ->
	gen_statem:call(Pid, {doingSolarCharge}).

getBatteryLevel(Pid) ->
	gen_statem:call(Pid, {getBattery}).

getDataElection(Pid) ->
	gen_statem:call(Pid, {getDataElection}).

getTimeToUser(Pid) ->
	gen_statem:call(Pid, {getTimeToUser}).

enablePathCharge(Pid) ->
	gen_statem:cast(Pid, {goToCharge}).

activateSolarCharge(Pid) ->
	gen_statem:cast(Pid, {activateSolCharge}).

updateQueue(Pid, Queue) ->
	gen_statem:cast(Pid, {updateQueue, Queue}).

areYouKillable(Pid) ->
	gen_statem:call(Pid, {areYouKillable}).

fullBattery(Pid) ->
	gen_statem:cast(Pid, {charged}).


%% ====================================================================
%% Automata Functions
%% ====================================================================

init(State) ->
	{ok, idle, State}.

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

%chiamato da listener per dire a utente tempo rimanente
handle_common({call,From}, {getTimeToUser}, OldState, State) ->
	TimeToUser = State#movingCarState.timeToUser,
	{next_state, OldState, State, [{reply,From,TimeToUser}]};

handle_common(cast, {crash}, _OldState, State) ->
	%avviso listener che avvisi utenti ...
	% Prepare data for special election -> [Battery, [{pid, from, to}, ...]] vengono inviati a ascoltatore e si arrangia lui
	{next_state, crash, State#movingCarState{tappe = [], currentUser = none,
											timeToUser = 0, costToLastDestination = 0}};

%la ricezione della queue con elezione fatta
%Queue = {costi,Tappe}
handle_common(cast, {updateQueue, Queue}, _OldState, State) ->
	printDebug(State,"Data Received:"),
	printDebug(State, Queue),
	{{Cost_1, Cost_2, _Cost_3}, TappeTarget, TappeColumn} = Queue,
	Time_To_User_Pos = State#movingCarState.costToLastDestination + Cost_1,
	CostToLastDest = State#movingCarState.costToLastDestination + Cost_1 + Cost_2,
	LastDestination = calculateLastDestination(TappeTarget),
	NewTappe = State#movingCarState.tappe ++ TappeTarget,
	{next_state, moving, State#movingCarState{tappe=NewTappe, 
											pathCol = TappeColumn, 
											lastDestination = LastDestination, 
											costToLastDestination = CostToLastDest, 
											timeToUser = Time_To_User_Pos}};
%gestione del tick
handle_common(info, {_From, tick}, OldState, State) ->
	if 
		(OldState == idle ) or (OldState == crash) -> 
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
	printDebug(State, "charged in moving"),
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
%di fatto consumi qua le tappe
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
	printDebug(State, "sto caricando"),
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
	printDebug(State, "sono crashato!"),
	keep_state_and_data;

crash({call,From}, {getDataElection}, State) ->
	Cost_To_last_Target = 0,
	Current_Target =  none,
	Battery_level = none,
	Packet = {Cost_To_last_Target, Current_Target, Battery_level, not_ok},
	{keep_state, State, [{reply,From,Packet}]};

crash(cast, {fixed}, State) ->
	printDebug(State, "sono stato riparato!"),
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
			if 
				ProssimoUtente /= TappaAttuale#tappa.user ->
					taxiServingAppId(ProssimoUtente, NewState),
					if 
						(ProssimoTipo =:= user_start) -> %caso in cui il prossimo utente è nel nodo attuale
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
	
