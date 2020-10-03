-module(macchina_moving_withRecords).
-compile(export_all).
-behaviour(gen_statem).
-define(HANDLE_COMMON,
    ?FUNCTION_NAME(T, C, D) -> handle_common(T, C,?FUNCTION_NAME, D)).
-include("records.hrl").
-include("globals.hrl").

-record(state, {tappe,
				currentUser = none,
				currentPos}).

callback_mode() ->
    [state_functions,state_enter].

start(InitialPos) ->
	{ok, Pid} = gen_statem:start_link(?MODULE, InitialPos, []),
	Pid.

init(InitialPos) ->
	tick_server:start_clock(?TICKTIME, [self()]),
	State = #state {tappe = [],
					currentUser = none,
					currentPos = InitialPos},
	{ok, idle, State}.

updateQueue(Pid, Queue) ->
    gen_statem:cast(Pid, {updateQueue, Queue}).

%update queue posseduta dal processo
updateQueuePid_alone(Pid,Queue) ->
	gen_statem:cast(Pid, {updateQueue, Queue}).

%handle_common(cast, {updateQueue, RcvQueue}, _OldState, State) ->
%	TappeAttuali = State#state.tappe,
%	NewTappe = TappeAttuali ++ RcvQueue,
%	my_util:println("ricevuto queue nuova"),
%	{next_state, moving, State#state{tappe=NewTappe}}.

handle_common(cast, {updateQueue, RcvQueue}, _OldState, State) ->
	{First, Second, Third} = RcvQueue,
	Actualpos = State#state.currentPos,
	{_Costi, Tappe} = city_map:calculate_path(First,Actualpos,Second,Third),
% Tolgo tappe colonnine  da TappeAttuali
%	TappeAttuali = State#state.tappe
	NewTappe = State#state.tappe ++ Tappe,
	{next_state, moving, State#state{tappe=NewTappe}}.

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
	PrimaTappa = hd(State#state.tappe),
	FirstUser = PrimaTappa#tappa.user,
	TipoTappa = PrimaTappa#tappa.type,
	TempoTappa = PrimaTappa#tappa.t,
	taxiServingAppId(FirstUser),
	ActualState = 
	if 
		(TipoTappa =:= user_start) and (TempoTappa == 0) -> %caso speciale in cui primo utente è nel nodo macchina
			arrivedInUserPosition(FirstUser),
			NuoveTappe = tl(State#state.tappe),
			State#state{currentUser = FirstUser,tappe = NuoveTappe};
		true -> State#state{currentUser = FirstUser}
	end,	
	printState(ActualState),
	{keep_state, ActualState};
	
moving(info, {_From, tick}, State) ->
	my_util:println("ricevuto tick...aggiorno spostamento"),
	TappeAttuali = State#state.tappe,
	TappaAttuale = hd(TappeAttuali),
	Time = TappaAttuale#tappa.t,
	NewTime = Time - 1,
	if 
		NewTime > 0 -> %sono nello spostamento fra due nodi, decremento il tempo
			TappaConSpostamento = TappaAttuale#tappa{t = NewTime},
			NuoveTappe = [TappaConSpostamento] ++ tl(TappeAttuali),
			NuovoStato =  State#state{tappe=NuoveTappe},
			printState(NuovoStato),
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
	?HANDLE_COMMON.

%return cambiamento stato macchina dopo essere arrivato a nuovo nodo e stampa se servo nuovo utente
calculateNewState(Stato, TappaAttuale, Tappe) ->
	NuoveTappe = tl(Tappe),
	Pos = TappaAttuale#tappa.node_name,
	sendPosToGps(Pos),
	if 
		NuoveTappe =:= [] -> Stato#state{tappe = [], currentUser = none, currentPos = Pos};
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
						   Stato#state{tappe = NuoveNuoveTappe, currentUser = ProssimoUtente, currentPos = Pos};
					   true ->
						   Stato#state{tappe = NuoveTappe, currentUser = ProssimoUtente, currentPos = Pos}
					end;
				true ->
					Stato#state{tappe = NuoveTappe, currentPos = Pos}
			end
	end.

sendPosToGps(_Position) ->
	foo.
  
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
	


%c(macchina_moving_withRecords),
%c(appUtente_flusso),
%f(),
%PidTaxi = macchina_moving_withRecords:start("a"),
%appUtente_flusso:start(PidTaxi).
	