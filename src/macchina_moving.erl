-module(macchina_moving).
-compile(export_all).
-behaviour(gen_statem).
-include("records.hrl").
-include("globals.hrl").
-import('utilities', [println/1, println/2, print_debug_message/1, print_debug_message/2, print_debug_message/3, print_debug_message_raw/1]).

callback_mode() -> [state_functions,state_enter].
%getDataElection, deve tornare {Cost_To_last_Target, Current_Target, Battery_Level} , Current_Target = pos attuale se fermo, dest se moving, next node se vai verso colonnina (stringa)
%diminuizione batteria a ogni tick, e anche cost to last target.
%Current_Target = next node se stai andando verso colonnina

%stato macchina in movimento:
-record(movingCarState, {
				tick_counter,
				pidListener,
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
start(InitialPos, PidListener) ->
	State = #movingCarState {
					tick_counter = 0,
					pidListener = PidListener,
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
areYouBusy(Pid) ->
	gen_statem:call(Pid, {areYouBusy}).

getBatteryLevel(Pid) ->
	gen_statem:call(Pid, {getBattery}).

getDataElection(Pid) ->
	gen_statem:call(Pid, {getDataElection}).

getTimeToUser(Pid) ->
	gen_statem:call(Pid, {getTimeToUser}).

enablePathCharge(Pid) ->
	gen_statem:cast(Pid, {goToCharge}).

updateQueue(Pid, Queue) ->
	gen_statem:cast(Pid, {updateQueue, Queue}).

fullBattery(Pid) ->
	gen_statem:cast(Pid, {charged}).

%% ====================================================================
%% Automata Functions
%% ====================================================================

init(State) ->
	{ok, idle, State}.

%automa batt mi dice di andare a caricare, accodo percorso colonnina
handle_common(cast, {goToCharge}, OldState, State) ->
	if OldState /= moving -> 
		    println("qualcosa sbagliato");
	   true ->
			println("aggiorno tappe aggiungendom patch carica"), %nb: l'aggiunta del path è implicito settando il flag
			{keep_state, State#movingCarState{mustCharge = true}}
	end;

%solo per V0
handle_common({call,From}, {areYouBusy}, OldState, State) ->
	Reply = if OldState == idle -> false;
	   		  true -> true
			end,
	{next_state, OldState, State, [{reply,From,Reply}]};
		
handle_common({call,From}, {getBattery}, OldState, State) ->
	Battery = State#movingCarState.batteryLevel,
	{next_state, OldState, State, [{reply,From,Battery}]};

handle_common({call,From}, {getTimeToUser}, OldState, State) ->
	TimeToUser = State#movingCarState.timeToUser,
	{next_state, OldState, State, [{reply,From,TimeToUser}]};

%ricezione richiesta senza elezione
handle_common(cast, {requestRcv, Request}, _OldState, State) ->
	{From, To, UserPid} = Request,
	Actualpos = State#movingCarState.currentPos,
	%calcola posizione finale nel caso di utenti in coda e usa quella in calculatePath
	{Costi, Tappe} = city_map:calculate_path(UserPid,Actualpos,From,To),
	CostoAlCliente = hd(Costi),
	CostoAlTarget = hd(tl(Costi)),
	% Tolgo tappe colonnine  da TappeAttuali
	%imposto tappe colonnina nell'attributo:
	PathColonnina = ["test"],
	%TappeAttuali = State#state.tappe
	LastDestination = calculateLastDestination(Tappe),
	NewTappe = State#movingCarState.tappe ++ Tappe,
	%NuovaBatteria = State#movingCarState.batteryLevel - (CostoAlCliente + CostoAlTarget),
	{next_state, moving, State#movingCarState{tappe=NewTappe, 
											pathCol = PathColonnina, 
											lastDestination = LastDestination, 
											costToLastDestination = CostoAlCliente + CostoAlTarget}};

%la ricezione della queue con elezione fatta
%Queue = {costi,Tappe}
handle_common(cast, {updateQueue, Queue}, _OldState, State) ->
	print_debug_message(State#movingCarState.pidListener, "W8 M8", none),
	print_debug_message(State#movingCarState.pidListener, "Data Received: ~w", [Queue]),
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

handle_common(info, {_From, tick}, OldState, State) ->
	if OldState == idle -> keep_state_and_data; %in idle ignora il tick
	   OldState == charging ->					%per ora aumenta di una carica ogni tick
		   printState(State),
		   NewBattery = State#movingCarState.batteryLevel + 1,
		   {keep_state, State#movingCarState{batteryLevel = NewBattery}};
	   true -> %se in moving o movingToCharge muoviti, consuma tappe
		ActualTickCounter = State#movingCarState.tick_counter,
		NewCounter = ActualTickCounter + 1,
		if (NewCounter >= ?TICKS_TO_MOVING) -> {keep_state, State#movingCarState{tick_counter = 0}, [{next_event,internal,move}]};
	   		true ->  {keep_state, State#movingCarState{tick_counter = NewCounter}}
		end
	 end.
		   

idle(enter, _OldState, _State) ->
	print_debug_message(self(), "Sono in idle", []),
	%printState(State),
	keep_state_and_data;

idle({call,From}, {getDataElection}, State) ->
	Cost_To_last_Target = 0,
	Current_Target = State#movingCarState.currentPos,
	Battery_level = State#movingCarState.batteryLevel,
	Packet = {Cost_To_last_Target, Current_Target, Battery_level},
	{keep_state, State, [{reply,From,Packet}]};

%gestisco tick ricevuti in idle ignorandoli
idle(info, {_From, tick}, _State) ->
	keep_state_and_data;
	
?HANDLE_COMMON.
	  
moving(enter, _OldState, State) ->
	print_debug_message(self(), "Sono in moving", []),
	PrimaTappa = hd(State#movingCarState.tappe),
	FirstUser = PrimaTappa#tappa.user,
	TipoTappa = PrimaTappa#tappa.type,
	TempoTappa = PrimaTappa#tappa.t,
	taxiServingAppId(FirstUser, State),
	ActualState = 
	if 
		(TipoTappa =:= user_start) and (TempoTappa == 0) -> %caso speciale in cui primo utente è nel nodo macchina
			arrivedInUserPosition(FirstUser, State),
			NuoveTappe = tl(State#movingCarState.tappe),
			State#movingCarState{currentUser = FirstUser,tappe = NuoveTappe};
		true -> State#movingCarState{currentUser = FirstUser}
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
	Time = TappaAttuale#tappa.t,
	NewTime = Time - 1,
	if 
		NewTime > 0 -> %sono nello spostamento fra due nodi, decremento il tempo
			TappaConSpostamento = TappaAttuale#tappa{t = NewTime},
			NuoveTappe = [TappaConSpostamento] ++ tl(TappeAttuali),
			NuovoStato =  State#movingCarState{tappe=NuoveTappe},
			io:format("-"),
			% print_debug_message(self(), "Sto raggiungendo: ~w", [TappaAttuale#tappa.node_name]),
			%printState(NuovoStato),
			{keep_state, NuovoStato#movingCarState{batteryLevel = NewBattery, costToLastDestination = NewCostToLastDest}};
		true -> %tempo = 0 , quindi ho raggiunto nodo nuovo
			println("raggiunto nuovo nodo"),
			TipoNodoAttuale = TappaAttuale#tappa.type,
			PersonaAttuale = TappaAttuale#tappa.user,
			NewState = calculateNewState(State, TappaAttuale, TappeAttuali),
			if  %controllo se sono arrivato al target oppure se sono arrivato da utente nuovo
				TipoNodoAttuale =:= user_target ->
					arrivedInTargetPosition(PersonaAttuale, State);
				TipoNodoAttuale =:= user_start ->
					arrivedInUserPosition(PersonaAttuale, State);
				true -> foo
			end,
			if 
				tl(TappeAttuali) =:= [] -> %ho finito il servizio
					if (State#movingCarState.mustCharge) -> %devo andare a caricare
							print_debug_message(self(), "Moving to charging", []),
						   	{next_state, movingToCharge, NewState#movingCarState{batteryLevel = NewBattery, costToLastDestination = 0}};
					    true -> 
						   	{next_state, idle, NewState#movingCarState{batteryLevel = NewBattery, costToLastDestination = 0}}
					end;
				true -> %ho ancora tappe
					printState(NewState),
					{keep_state, NewState#movingCarState{batteryLevel = NewBattery, costToLastDestination = NewCostToLastDest}}
			end
	end;

moving({call,From}, {getDataElection}, State) ->
	Cost_To_last_Target = State#movingCarState.costToLastDestination,
	Current_Target = State#movingCarState.lastDestination,
	Battery_level = State#movingCarState.batteryLevel,
	Packet = {Cost_To_last_Target, Current_Target, Battery_level},
	{keep_state, State, [{reply,From,Packet}]};

moving(cast, {crash}, State) ->
	print_debug_message(self(), "Incidente!", []),
	sendUserCrashEvent(State#movingCarState.currentUser, State),
	{next_state, crash, State#movingCarState{tappe = [],currentUser = none}};
	?HANDLE_COMMON.
	  
%qua consumo le tappe verso colonnina settate nell'apposito attributo
movingToCharge(enter, _OldState, State) ->
	println("Sono in moving verso la colonnina"),
	%controllo se la colonnina è qua dove sono
	PrimaTappa = hd(State#movingCarState.pathCol),
	TipoTappa = PrimaTappa#tappa.type,
	TempoTappa = PrimaTappa#tappa.t,
	if 
		(TipoTappa =:= column) and (TempoTappa == 0) -> %caso speciale in cui colonnina è dove sono
			{next_state, charging, State#movingCarState{pathCol = [], costToLastDestination = 0}};
		true ->
			keep_state_and_data
	end;

movingToCharge({call,From}, {getDataElection}, State) ->
	Cost_To_last_Target = State#movingCarState.costToLastDestination,
	Current_Target =  State#movingCarState.lastDestination,
	Battery_level = State#movingCarState.batteryLevel,
	Packet = {Cost_To_last_Target, Current_Target, Battery_level},
	{keep_state, State, [{reply,From,Packet}]};

%similar to moving
%di fatto consumi qua le tappe
movingToCharge(internal, move, State) ->
	NewBattery = State#movingCarState.batteryLevel - 1,
	TappeAttuali = State#movingCarState.pathCol,		 %cambia qua sostanzialmente
	TappaAttuale = hd(TappeAttuali),
	NextHop = TappaAttuale#tappa.node_name,
	Time = TappaAttuale#tappa.t,
	NewTime = Time - 1,
	
	if 
		NewTime > 0 -> %sono nello spostamento fra due nodi, decremento il tempo
			TappaConSpostamento = TappaAttuale#tappa{t = NewTime},
			NuoveTappeCol = [TappaConSpostamento] ++ tl(TappeAttuali),
			io:format("-"),
			%il costToLastDest nel caso di spostamento verso col è il costo nexthop
			{keep_state, State#movingCarState{pathCol = NuoveTappeCol, 
											batteryLevel = NewBattery, 
											costToLastDestination = NewTime, 
											lastDestination = NextHop}}; 
	true -> %tempo = 0 , quindi ho raggiunto nodo nuovo
			println("raggiunto nuovo nodo"),
			NewPos = TappaAttuale#tappa.node_name,
			sendPosToGps(NewPos, State),
			TipoNodoAttuale = TappaAttuale#tappa.type,
			if  %controllo in che tipo di nodo sono arrivato (intermedio o colonnina)
				TipoNodoAttuale =:= column -> %arrivato alla colonnina
					println("colonnina raggiunta!"),
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
  
charging(enter, _OldState, _State) ->
	println("sto caricando"),
	keep_state_and_data;

charging(cast, {charged}, State) ->
	println("finito di caricare, sono al massimo"),
	{next_state, idle, State};

?HANDLE_COMMON.
  
crash(enter, _OldState, _State) ->
	exit(crash);

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
		NuoveTappe =:= [] -> Stato#movingCarState{tappe = [], currentUser = none, currentPos = Pos, lastDestination = Pos};
		true ->
			TipoNodoAttuale = TappaAttuale#tappa.type,
			NewState =
				if TipoNodoAttuale =:= user_start -> Stato#movingCarState{timeToUser = 0};
				   true -> Stato 
				end,
			ProssimaTappa = hd(NuoveTappe),
			ProssimoUtente = ProssimaTappa#tappa.user,
			ProssimoTipo = ProssimaTappa#tappa.type,
			if 
				ProssimoUtente /= TappaAttuale#tappa.user ->
					taxiServingAppId(ProssimoUtente, NewState),
					if (ProssimoTipo =:= user_start) -> %caso in cui il prossimo utente è nel nodo attuale
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

sendUserCrashEvent(UserPid,S) ->
	ListenerPid = S#movingCarState.pidListener,
	macchina_ascoltatore:sendToEsternalAutomata(ListenerPid, UserPid, crash).
  
sendPosToUser(UserPid, Position, S) ->
	ListenerPid = S#movingCarState.pidListener,
	macchina_ascoltatore:sendToEsternalAutomata(ListenerPid, UserPid, {newNodeReached, Position}).

taxiServingAppId(UserPid, S) ->
	ListenerPid = S#movingCarState.pidListener,
	macchina_ascoltatore:sendToEsternalAutomata(ListenerPid, UserPid, taxiServingYou).

arrivedInUserPosition(UserPid, S) ->
	ListenerPid = S#movingCarState.pidListener,
	macchina_ascoltatore:sendToEsternalAutomata(ListenerPid, UserPid, arrivedUserPosition).

arrivedInTargetPosition(UserPid, S) ->
	ListenerPid = S#movingCarState.pidListener,
	macchina_ascoltatore:sendToEsternalAutomata(ListenerPid, UserPid, arrivedTargetPosition).

printState(State) ->
	utilities:print_debug_message(self(), "Stato: " ++ [?TILDE_CHAR] ++ "p", State).
	


%make:all(),
%f(),
%PidTaxi = macchina_moving:start("a"),
%appUtente_flusso:start(PidTaxi).
	