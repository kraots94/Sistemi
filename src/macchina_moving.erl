-module(macchina_moving).
-compile(export_all).
-behaviour(gen_statem).
-include("records.hrl").
-include("globals.hrl").

callback_mode() -> [state_functions,state_enter].

%pidWirelessCard
%				 tappe,
%				 currentUser,
%				 currentPos

%% ====================================================================
%% API functions
%% ====================================================================
start(InitialPos, PidWireless) ->
	State = #movingCarState {
					pidWirelessCard = PidWireless,
					tappe = [],
					currentUser = none,
					currentPos = InitialPos,
					batteryLevel = 100},
	{ok, Pid} = gen_statem:start_link(?MODULE,State, []),
	Pid.

%update queue posseduta dal processo
updateQueuePid_alone(Pid,Queue) ->
	gen_statem:cast(Pid, {updateQueue, Queue}).

crash(Pid) ->
	gen_statem:cast(Pid, {crash}).

%NB: questo torna la risposta al chiamante, se imposti timer e non c'è risposta invece la chiamata fallisce
areYouBusy(Pid) ->
	gen_statem:call(Pid, {areYouBusy}).

%% ====================================================================
%% Automata Functions
%% ====================================================================

init(State) ->
	%qua sara' da togliere quando farai ascoltatore
	tick_server:start_clock(?TICKTIME, [self()]),
	wireless_card_server:sendPosToGps(State#movingCarState.pidWirelessCard, State#movingCarState.currentPos),
	{ok, idle, State}.

handle_common(cast, {updateQueue, RcvQueue}, _OldState, State) ->
	{UserPid, From, To} = RcvQueue,
	Actualpos = State#movingCarState.currentPos,
%calcola posizione finale nel caso di utenti in coda e usa quella in calculatePath
	{Costi, Tappe} = city_map:calculate_path(UserPid,Actualpos,From,To),
	CostoAlCliente = hd(Costi),
	CostoAlTarget = hd(tl(Costi)),
	my_util:println("costi", CostoAlCliente),
% Tolgo tappe colonnine  da TappeAttuali
%	TappeAttuali = State#state.tappe
	NewTappe = State#movingCarState.tappe ++ Tappe,
	NuovaBatteria = State#movingCarState.batteryLevel - (CostoAlCliente + CostoAlTarget),
	{next_state, moving, State#movingCarState{tappe=NewTappe, batteryLevel = NuovaBatteria}};

handle_common({call,From}, {areYouBusy}, OldState, State) ->
	Reply = if OldState == idle -> false;
	   		  true -> true
			end,
	{next_state, OldState, State, [{reply,From,Reply}]};
		
	
%assumendo sia in idle (V0)
%torna tupla con {cc, crdt}
handle_common(cast, {calculateElectionValues, ClientNode, Target}, _OldState, State) ->
	Actualpos = State#movingCarState.currentPos,
	{Costi, _Tappe} = city_map:calculate_path(self(),Actualpos,ClientNode,Target),
	CostoAlClient = hd(Costi),
	CostoAllaDestinazione = hd(tl(Costi)),
	%Pvcr = ora non c'e'
	BatteriaAttuale = State#movingCarState.batteryLevel,
	CaricaDopoSpostamento = BatteriaAttuale - (CostoAlClient + CostoAllaDestinazione),
	{CostoAlClient, CaricaDopoSpostamento}.
	

idle(enter, _OldState, Stato) ->
	my_util:println("sono in idle..."),
	printState(Stato),
	keep_state_and_data;

%gestisco tick ricevuti in idle ignorandoli
idle(info, {_From, tick}, _Stato) ->
	keep_state_and_data;
	
?HANDLE_COMMON.
	  
moving(enter, _OldState, State) ->
	my_util:println("mi sto spostando..."),
	PrimaTappa = hd(State#movingCarState.tappe),
	FirstUser = PrimaTappa#tappa.user,
	TipoTappa = PrimaTappa#tappa.type,
	TempoTappa = PrimaTappa#tappa.t,
	taxiServingAppId(FirstUser),
	ActualState = 
	if 
		(TipoTappa =:= user_start) and (TempoTappa == 0) -> %caso speciale in cui primo utente è nel nodo macchina
			arrivedInUserPosition(FirstUser),
			NuoveTappe = tl(State#movingCarState.tappe),
			State#movingCarState{currentUser = FirstUser,tappe = NuoveTappe};
		true -> State#movingCarState{currentUser = FirstUser}
	end,	
	printState(ActualState),
	{keep_state, ActualState};
	
%gestione tick in movimento
moving(info, {_From, tick}, State) ->
	%my_util:println("ricevuto tick...aggiorno spostamento"),
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
			%my_util:println("sto raggiungendo: ", TappaAttuale#tappa.node_name),
			%printState(NuovoStato),
			{keep_state, NuovoStato};
		true -> %tempo = 0 , quindi ho raggiunto nodo nuovo
			my_util:println("raggiunto nuovo nodo"),
			TipoNodoAttuale = TappaAttuale#tappa.type,
			PersonaAttuale = TappaAttuale#tappa.user,
			NewState = calculateNewState(State, TappaAttuale, TappeAttuali),
			if  %controllo se sono arrivato al target oppure se sono arrivato da utente nuovo
				TipoNodoAttuale =:= user_target ->
					arrivedInTargetPosition(PersonaAttuale);
				TipoNodoAttuale =:= user_start ->
					arrivedInUserPosition(PersonaAttuale);
				true -> foo
			end,
			if 
				tl(TappeAttuali) =:= [] -> %ho finito il servizio
					{next_state, idle, NewState};
				true -> %ho ancora tappe
					printState(NewState),
					{keep_state, NewState}
			end
	end;
	
moving(cast, {crash}, State) ->
	my_util:println("ho fatto un incidente!"),
	sendUserCrashEvent(State#movingCarState.currentUser),
	{next_state, crash, State#movingCarState{tappe = [],currentUser = none}};
	?HANDLE_COMMON.
	  
crash(enter, _OldState, _Stato) ->
	exit(crash);
	?HANDLE_COMMON.
%% ====================================================================
%% Internal functions
%% ====================================================================

%return cambiamento stato macchina dopo essere arrivato a nuovo nodo e invia relativi eventi a utenti nuovi o ragginti
calculateNewState(Stato, TappaAttuale, Tappe) ->
	NuoveTappe = tl(Tappe),
	Pos = TappaAttuale#tappa.node_name,
	sendPosToUser(Stato#movingCarState.currentUser,Pos),
	wireless_card_server:sendPosToGps(Stato#movingCarState.pidWirelessCard, Pos),
	if 
		NuoveTappe =:= [] -> Stato#movingCarState{tappe = [], currentUser = none, currentPos = Pos};
		true ->
			ProssimaTappa = hd(NuoveTappe),
			ProssimoUtente = ProssimaTappa#tappa.user,
			ProssimoTipo = ProssimaTappa#tappa.type,
			if 
				ProssimoUtente /= TappaAttuale#tappa.user ->
					taxiServingAppId(ProssimoUtente),
					if (ProssimoTipo =:= user_start) -> %caso in cui il prossimo utente è nel nodo attuale
						   arrivedInUserPosition(ProssimoUtente),
						   NuoveNuoveTappe = tl(NuoveTappe),
						   Stato#movingCarState{tappe = NuoveNuoveTappe, currentUser = ProssimoUtente, currentPos = Pos};
					   true ->
						   Stato#movingCarState{tappe = NuoveTappe, currentUser = ProssimoUtente, currentPos = Pos}
					end;
				true ->
					Stato#movingCarState{tappe = NuoveTappe, currentPos = Pos}
			end
	end.

sendUserCrashEvent(PidUser) ->
	gen_statem:cast(PidUser, crash).
  
sendPosToUser(UserPid, Position) ->
	appUtente_flusso:updatePosition(UserPid, Position).
  
taxiServingAppId(User) ->
	gen_statem:cast(User,taxiServingYou).
	%my_util:println("sto servendo: ", User).

arrivedInUserPosition(User) ->
	gen_statem:cast(User,arrivedUserPosition).
	%my_util:println("sono arrivato da", User).

arrivedInTargetPosition(User) ->
	gen_statem:cast(User,arrivedTargetPosition).
	%my_util:println("ho servito: ", User).


printState(State) ->
	my_util:println("situazione Stato:", State).
	

%make:all(),
%f(),
%PidTaxi = macchina_moving:start("a"),
%appUtente_flusso:start(PidTaxi).
	