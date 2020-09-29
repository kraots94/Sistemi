-module(macchina_moving_withRecords).
-compile(export_all).
-behaviour(gen_statem).
-define(TICKTIME, 2).
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

start() ->
	{ok, Pid} = gen_statem:start(?MODULE, foo, []),
	Pid.

callback_mode() ->
    [state_functions,state_enter].

init(foo) ->
	tick_server:start(?TICKTIME, self()),
	State = #state {tappe = [],
					currentUser = "",
					currentPos = {0,0}},
	{ok, idle, State}.

updateQueue(Pid, Queue) ->
    gen_statem:cast(Pid, {updateQueue, Queue}).

updateQueue1(Pid) ->
	Record1 = #tappa {user = "pippo", tipo = "partenza", tempo = 3, posizione = {1,2}},
	Record2 = #tappa {user = "pippo", tipo = "normale", tempo = 5, posizione = {5,7}},
	Record3 = #tappa {user = "pippo", tipo = "arrivo", tempo = 4, posizione = {5,7}},
	Record4 = #tappa {user = "pluto", tipo = "partenza", tempo = 4, posizione = {5,7}},
	Record5 = #tappa {user = "pluto", tipo = "arrivo", tempo = 3, posizione = {123,123}},
	Queue = [Record1,Record2,Record3,Record4,Record5],
	gen_statem:cast(Pid, {updateQueue, Queue}).

handle_common(cast, {updateQueue, RcvQueue}, _OldState, State) ->
	TappeAttuali = State#state.tappe,
	NewTappe = TappeAttuali ++ RcvQueue,
	my_util:println("ricevuto queue nuova"),
	{next_state, moving, State#state{tappe=NewTappe}}.

idle(enter, _OldState, _Stato) ->
	my_util:println("sono in idle"),
	keep_state_and_data;

%gestisco tick ricevuti in idle ignorandoli
idle(info, {_From, tick}, _Stato) ->
	keep_state_and_data;
	?HANDLE_COMMON.
	  
moving(enter, _OldState, State) ->
	my_util:println("mi sto spostando..."),
	keep_state_and_data;
	
moving(info, {_From, tick}, State) ->
	my_util:println("ricevuto tick...aggiorno spostamento"),
	my_util:println("situazione coda:", State#state.tappe),
	TappeAttuali = State#state.tappe,
	if
		length(TappeAttuali) == 0 -> {next_state, idle, State#state{currentUser=""}};
		true ->
			TappaAttuale = hd(TappeAttuali),
			NewState = checkTappa(TappaAttuale, State),
			Time = TappaAttuale#tappa.tempo,
			if 
				Time == 1 ->  {keep_state, NewState#state{tappe=tl(TappeAttuali)}};
				true -> 
					NuovoTempo = Time - 1,
					TappaConSpostamento = TappaAttuale#tappa{tempo = NuovoTempo},
					NuoveTappe = [TappaConSpostamento] ++ tl(TappeAttuali),
					{keep_state, NewState#state{tappe=NuoveTappe}} 
			end
	end;
	?HANDLE_COMMON.

checkTappa(Tappa, State) -> 
		Utente = Tappa#tappa.user,
		%Tipo = Tappa#tappa.tipo,
		if 
			Utente /= State#state.currentUser -> 
				my_util:println("sto servendo:", Utente),
				State#state{currentUser = Utente};
			true -> State
		end.
	