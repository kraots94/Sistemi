-module(macchina_moving_withRecords).
-compile(export_all).
-behaviour(gen_statem).
-include("records.hrl").
-include("globals.hrl").

callback_mode() -> [state_functions,state_enter].

%% ====================================================================
%% API functions
%% ====================================================================
start(InitialPos, PidWireless) ->
	State = #movingCarState {
					pidWirelessCard = PidWireless,
					tappe = [],
					currentUser = none,
					currentPos = InitialPos},
	{ok, Pid} = gen_statem:start_link(?MODULE,State, []),
	sendPosToGps(PidWireless, InitialPos),
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
	tick_server:start_clock(?TICKTIME, [self()]),
	{ok, idle, State}.

handle_common(cast, {updateQueue, RcvQueue}, _OldState, State) ->
	{First, Second, Third} = RcvQueue,
	Actualpos = State#movingCarState.currentPos,
%calcola posizione finale nel caso di utenti in coda e usa quella in calculatePath
	{_Costi, Tappe} = city_map:calculate_path(First,Actualpos,Second,Third),
% Tolgo tappe colonnine  da TappeAttuali
%	TappeAttuali = State#state.tappe
	NewTappe = State#movingCarState.tappe ++ Tappe,
	{next_state, moving, State#movingCarState{tappe=NewTappe}};

handle_common({call,From}, {areYouBusy}, OldState, State) ->
	Reply = if OldState == idle -> false;
	   		  true -> true
			end,
	{next_state, OldState, State, [{reply,From,Reply}]}.
		
	
	

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
	sendPosToGps(Stato#movingCarState.pidWirelessCard, Pos),
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
  
sendPosToGps(WirelessCardPid, Position) ->
	foo.
	%WirelessCardPid ! {self(), {setPosition, Position}}.

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
%PidTaxi = macchina_moving_withRecords:start("a"),
%appUtente_flusso:start(PidTaxi).
	