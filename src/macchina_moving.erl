-module(macchina_moving).
-compile(export_all).
-behaviour(gen_statem).
-include("records.hrl").
-include("globals.hrl").
-import('utilities', [print_debug_message/1, print_debug_message/2, print_debug_message/3, print_debug_message_raw/1]).

callback_mode() -> [state_functions,state_enter].
%getDataElection, deve tornare {Cost_To_last_Target, Current_Target, Battery_Level} , Current_Target = pos attuale se fermo, dest se moving, next node se vai verso colonnina (stringa)
%diminuizione batteria a ogni tick, e anche cost to last target.
%Current_Target = next node se stai andando verso colonnina

%stato macchina in movimento:
-record(movingCarState, {
				pidListener,
				tappe,
				currentUser = none,
				currentPos,
				batteryLevel,
				pathCol,
				mustCharge}).

%% ====================================================================
%% API functions
%% ====================================================================
start(InitialPos, PidListener) ->
	State = #movingCarState {
					pidListener = PidListener,
					tappe = [],
					currentUser = none,
					currentPos = InitialPos,
					batteryLevel = 100,
					pathCol = [],
					mustCharge = false},
	{ok, Pid} = gen_statem:start_link(?MODULE,State, []),
	Pid.

crash(Pid) ->
	gen_statem:cast(Pid, {crash}).

%NB: questo torna la risposta al chiamante, se imposti timer e non c'è risposta invece la chiamata fallisce
areYouBusy(Pid) ->
	gen_statem:call(Pid, {areYouBusy}).

getBatteryLevel(Pid) ->
	gen_statem:call(Pid, {getBattery}).

enablePathCharge(Pid) ->
	gen_statem:cast(Pid, {goToCharge}).

%% ====================================================================
%% Automata Functions
%% ====================================================================

init(State) ->
	{ok, idle, State}.

%automa batt mi dice di andare a caricare, accodo percorso colonnina
handle_common(cast, {goToCharge}, OldState, State) ->
	if OldState /= moving -> 
			print_debug_message(self(), "qualcosa sabagliato", []);
	   true ->
			print_debug_message(self(), "aggiorno tappe aggiungendo patch carica", []),
			{keep_state, State#movingCarState{mustCharge = true}}
	end;

handle_common(cast, {requestRcv, Request}, _OldState, State) ->
	{From, To, UserPid} = Request,
	Actualpos = State#movingCarState.currentPos,
%calcola posizione finale nel caso di utenti in coda e usa quella in calculatePath
	{Costi, Tappe} = city_map:calculate_path(UserPid,Actualpos,From,To),
	CostoAlCliente = hd(Costi),
	_CostoAlTarget = hd(tl(Costi)),
	print_debug_message(self(), "Costi: ~p", [CostoAlCliente]),

% Tolgo tappe colonnine  da TappeAttuali
%imposto tappe colonnina nell'attributo:
	PathColonnina = ["test"],
%	TappeAttuali = State#state.tappe
	NewTappe = State#movingCarState.tappe ++ Tappe,
	%NuovaBatteria = State#movingCarState.batteryLevel - (CostoAlCliente + CostoAlTarget),
	{next_state, moving, State#movingCarState{tappe=NewTappe, pathCol = PathColonnina}};

%la ricezione della queue con elezione fatta
%Queue = {costi,Tappe}
handle_common(cast, {updateQueue, Queue}, _OldState, State) ->
	{Costi, Tappe} = Queue,
	NewTappe = State#movingCarState.tappe ++ Tappe,
	%estrappolo path colonnina dalle tappe
	PathColonnina = ["test"],
	{next_state, moving, State#movingCarState{tappe=NewTappe, pathCol = PathColonnina}};

handle_common({call,From}, {areYouBusy}, OldState, State) ->
	Reply = if OldState == idle -> false;
	   		  true -> true
			end,
	{next_state, OldState, State, [{reply,From,Reply}]};
		
handle_common({call,From}, {getBattery}, OldState, State) ->
	Battery = State#movingCarState.batteryLevel,
	{next_state, OldState, State, [{reply,From,Battery}]};
	
%assumendo sia in idle (V0)
%torna tupla con {cc, crdt, Tappe}
%le tappe sono incluse perche' in caso di vincita automa elettore le torna invia qua aggiornando coda
handle_common(cast, {calculateElectionValues, ClientNode, Target}, _OldState, State) ->
	Actualpos = State#movingCarState.currentPos,
	{Costi, Tappe} = city_map:calculate_path(self(),Actualpos,ClientNode,Target),
	CostoAlClient = hd(Costi),
	CostoAllaDestinazione = hd(tl(Costi)),
	%CostoAllaColonnina = ora non c'e'
	BatteriaAttuale = State#movingCarState.batteryLevel,
	CaricaDopoSpostamento = BatteriaAttuale - (CostoAlClient + CostoAllaDestinazione),
	{CostoAlClient, CaricaDopoSpostamento, Tappe};

%aggiornamento queue dopo che elezione sarà finita
handle_common(cast, {updateQueue2, _RcvQueue}, _OldState, _State) ->
	%gli arriva {Tappe, CaricaDopoSpostamento}
	%sottraggo alla batteria
	%tolgo percorso colonnina (e metto in una variabile stato dedicata maybe) cosi' che quando automa batteria mi dice io lo rimetto in coda
 	%vai in moving con tappa.
	toDo.

idle(enter, _OldState, State) ->
	print_debug_message(self(), "Sono in idle", []),
	printState(State),
	keep_state_and_data;

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
	
%gestione tick in movimento
moving(info, {_From, tick}, State) ->
	% print_debug_message(self(), "Ricevuto tick...aggiorno spostamento", []),
	NewBattery = State#movingCarState.batteryLevel - 1,
	TappeAttuali = State#movingCarState.tappe,
	TappaAttuale = hd(TappeAttuali),
	Time = TappaAttuale#tappa.t,
	NewTime = Time - 1,
	if 
		NewTime > 0 -> %sono nello spostamento fra due nodi, decremento il tempo
			TappaConSpostamento = TappaAttuale#tappa{t = NewTime},
			NuoveTappe = [TappaConSpostamento] ++ tl(TappeAttuali),
			NuovoStato =  State#movingCarState{tappe=NuoveTappe},
			print_debug_message_raw("-"),
			% print_debug_message(self(), "Sto raggiungendo: ~w", [TappaAttuale#tappa.node_name]),
			%printState(NuovoStato),
			{keep_state, NuovoStato#movingCarState{batteryLevel = NewBattery}};
		true -> %tempo = 0 , quindi ho raggiunto nodo nuovo
			print_debug_message(self(), "Raggiunto nuovo nodo", []),
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
						   	{next_state, movingToCharge, NewState#movingCarState{batteryLevel = NewBattery}};
					    true -> 
						   	{next_state, idle, NewState#movingCarState{batteryLevel = NewBattery}}
					end;
				true -> %ho ancora tappe
					printState(NewState),
					{keep_state, NewState#movingCarState{batteryLevel = NewBattery}}
			end
	end;
	
moving(cast, {crash}, State) ->
	print_debug_message(self(), "Incidente!", []),
	sendUserCrashEvent(State#movingCarState.currentUser, State),
	{next_state, crash, State#movingCarState{tappe = [],currentUser = none}};
	?HANDLE_COMMON.
	  
movingToCharge(enter, _OldState, _State) ->
	print_debug_message(self(), "Sono in moving verso la colonnina", []),
	keep_state_and_data;
	
	  
movingToCharge(info, {_From, tick}, _State) ->
	keep_state_and_data;

?HANDLE_COMMON.
  

crash(enter, _OldState, _State) ->
	exit(crash);
	?HANDLE_COMMON.
%% ====================================================================
%% Internal functions
%% ====================================================================

%return cambiamento stato macchina dopo essere arrivato a nuovo nodo e invia relativi eventi a utenti nuovi o ragginti
calculateNewState(Stato, TappaAttuale, Tappe) ->
	NuoveTappe = tl(Tappe),
	Pos = TappaAttuale#tappa.node_name,
	sendPosToUser(Stato#movingCarState.currentUser,Pos,Stato),
	sendPosToGps(Pos, Stato),
	if 
		NuoveTappe =:= [] -> Stato#movingCarState{tappe = [], currentUser = none, currentPos = Pos};
		true ->
			ProssimaTappa = hd(NuoveTappe),
			ProssimoUtente = ProssimaTappa#tappa.user,
			ProssimoTipo = ProssimaTappa#tappa.type,
			if 
				ProssimoUtente /= TappaAttuale#tappa.user ->
					taxiServingAppId(ProssimoUtente, Stato),
					if (ProssimoTipo =:= user_start) -> %caso in cui il prossimo utente è nel nodo attuale
						   arrivedInUserPosition(ProssimoUtente, Stato),
						   NuoveNuoveTappe = tl(NuoveTappe),
						   Stato#movingCarState{tappe = NuoveNuoveTappe, currentUser = ProssimoUtente, currentPos = Pos};
					   true ->
						   Stato#movingCarState{tappe = NuoveTappe, currentUser = ProssimoUtente, currentPos = Pos}
					end;
				true ->
					Stato#movingCarState{tappe = NuoveTappe, currentPos = Pos}
			end
	end.

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
	print_debug_message(self(), "Car State: ~w", [State]).
	

%make:all(),
%f(),
%PidTaxi = macchina_moving:start("a"),
%appUtente_flusso:start(PidTaxi).
	