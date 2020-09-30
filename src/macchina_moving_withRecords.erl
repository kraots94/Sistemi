-module(macchina_moving_withRecords).
-compile(export_all).
-behaviour(gen_statem).
-define(TICKTIME, 1).
-define(HANDLE_COMMON,
    ?FUNCTION_NAME(T, C, D) -> handle_common(T, C,?FUNCTION_NAME, D)).

-record(tappa, {user = "",
				tipo = "",
				tempo = 0,
				posizione = {0,0}
				}).

-record(state, {tappe,
				currentUser = "",
				currentPos = {0,0}}).

start(InitialPos) ->
	{ok, Pid} = gen_statem:start(?MODULE, InitialPos, []),
	Pid.

callback_mode() ->
    [state_functions,state_enter].

init(InitialPos) ->
	tick_server:start_clock(?TICKTIME, [self()]),
	State = #state {tappe = [],
					currentUser = "",
					currentPos = InitialPos},
	{ok, idle, State}.

updateQueue(Pid, Queue) ->
    gen_statem:cast(Pid, {updateQueue, Queue}).

updateQueue1(Pid) ->
	Record1 = #tappa {user = "pippo", tipo = "normale", tempo = 3, posizione = {1,2}},
	Record2 = #tappa {user = "pippo", tipo = "partenza", tempo = 5, posizione = {5,7}},
	Record7 = #tappa {user = "pippo", tipo = "normale", tempo = 3, posizione = {1,2}},
	Record3 = #tappa {user = "pippo", tipo = "arrivo", tempo = 4, posizione = {5,7}},
	Record4 = #tappa {user = "pluto", tipo = "normale", tempo = 4, posizione = {5,7}},
	Record5 = #tappa {user = "pluto", tipo = "partenza", tempo = 3, posizione = {123,123}},
	Record6 = #tappa {user = "pluto", tipo = "arrivo", tempo = 3, posizione = {123,123}},
	Queue = [Record1,Record2,Record7,Record3,Record4,Record5,Record6],
	gen_statem:cast(Pid, {updateQueue, Queue}).

updateQueue2(Pid) ->
	Record1 = #tappa {user = "topolino", tipo = "partenza", tempo = 1, posizione = {1,2}},
	Record5 = #tappa {user = "topolino", tipo = "arrivo", tempo = 1, posizione = {123,123}},
	Queue = [Record1,Record5],
	gen_statem:cast(Pid, {updateQueue, Queue}).


handle_common(cast, {updateQueue, RcvQueue}, _OldState, State) ->
	TappeAttuali = State#state.tappe,
	NewTappe = TappeAttuali ++ RcvQueue,
	my_util:println("ricevuto queue nuova"),
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
	ActualState = State#state{currentUser = FirstUser},
	printState(ActualState),
	{keep_state, ActualState};
	
moving(info, {_From, tick}, State) ->
	my_util:println("ricevuto tick...aggiorno spostamento"),
	TappeAttuali = State#state.tappe,
	TappaAttuale = hd(TappeAttuali),
	Time = TappaAttuale#tappa.tempo,
	NewTime = Time - 1,
	if 
		NewTime > 0 -> %sono nello spostamento fra due nodi, decremento il tempo
			TappaConSpostamento = TappaAttuale#tappa{tempo = NewTime},
			NuoveTappe = [TappaConSpostamento] ++ tl(TappeAttuali),
			NuovoStato =  State#state{tappe=NuoveTappe},
			printState(NuovoStato),
			{keep_state, NuovoStato};
		true -> %tempo = 0 , quindi ho raggiunto nodo nuovo
			my_util:println("raggiunto nuovo nodo"),
			TipoNodoAttuale = TappaAttuale#tappa.tipo,
			PersonaAttuale = TappaAttuale#tappa.user,
			if  %controllo se sono arrivato al target oppure se sono arrivato da utente nuovo
				TipoNodoAttuale =:= "arrivo" ->
					arrivedInTargetPosition(PersonaAttuale);
				TipoNodoAttuale =:= "partenza" ->
					arrivedInUserPosition(PersonaAttuale);
				true -> foo
			end,
			NewState = calculateNewState(State, TappaAttuale, TappeAttuali),
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
	Pos = TappaAttuale#tappa.posizione,
	sendPosToGps(Pos),
	if 
		NuoveTappe =:= [] -> Stato#state{tappe = [], currentUser = "", currentPos = Pos};
		true ->
			ProssimaTappa = hd(NuoveTappe),
			ProssimoUtente = ProssimaTappa#tappa.user,
			if 
				ProssimoUtente /= TappaAttuale#tappa.user ->
					taxiServingAppId(ProssimoUtente),
					Stato#state{tappe = NuoveTappe, currentUser = ProssimoUtente, currentPos = Pos};
				true ->
					Stato#state{tappe = NuoveTappe, currentPos = Pos}
			end
	end.

sendPosToGps(_Position) ->
	foo.
  
arrivedInTargetPosition(User) ->
	my_util:println("ho servito: ", User).

arrivedInUserPosition(User) ->
	my_util:println("sono arrivato da", User).

taxiServingAppId(User) ->
		my_util:println("sto servendo: ", User).

printState(State) ->
	my_util:println("situazione Stato:", State).
		
%testing seq
%c(macchina_moving_withRecords),
%f(),
%Pid = macchina_moving_withRecords:start({44,33}),
%macchina_moving_withRecords:updateQueue1(Pid).
	