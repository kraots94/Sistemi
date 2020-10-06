%% @author Agnul
%% @doc @todo Add description to macchina_ascoltatore.

-module(macchina_ascoltatore).
-compile(export_all).
-behaviour(gen_statem).
-include("records.hrl").
-include("globals.hrl").

callback_mode() -> [state_functions].

%% ====================================================================
%% API functions
%% ====================================================================

%si crea con {"nodo iniziale", PidWireless, mappa}
start(InitData) ->
	{ok, Pid} = gen_statem:start_link(?MODULE,InitData, []),
	Pid.

beginElection(Pid) ->
	gen_statem:call(Pid, {beginElectionUser}).

%% ====================================================================
%% Automata functions
%% ====================================================================

init(InitData) -> 
	{InitialPos, PidWireless, City_Map} = InitData,
	PidMoving = macchina_moving:start(InitialPos, PidWireless),
	PidBattery  = macchina_batteria:start(PidMoving),
	PidElection = macchina_elezione:start(self(), PidWireless, City_Map),
	State = #taxiListenerState {
					pidMoving   = PidMoving,
					pidBattery  = PidBattery,
					pidElection = PidElection
			},
	my_util:printList("Pid creati:", [PidMoving, PidBattery, PidElection]),
	tick_server:start_clock(?TICKTIME, [self(),PidMoving,PidBattery,PidElection]),
	{ok, idle, State}.
	
idle(info, {_From, tick}, _Stato) ->
	keep_state_and_data; %per ora non fare nulla
	
%begin election ricevuto da app utente
%Data = {From,To,PidAppUser}
idle(cast, {beginElection, Data}, Stato) ->
	{From,To,PidAppUser} = Data,
	%piazzo dentro anche pid moving cosi' che automa prenda queue se serve
	NewData = {From,To,PidAppUser,self(),Stato#taxiListenerState.pidMoving},
	PidElezione = Stato#taxiListenerState.pidElection,
	gen_statem:cast(PidElezione, {beginElection,NewData}),
	{next_state, listen_election, Stato};

%partecipate election ricevuto da altra macchina
idle(cast, {partecipateElection, Data}, Stato) ->
	{From,To,PidParent} = Data,
	%piazzo dentro anche pid moving cosi' che automa prenda queue se serve
	NewData = {From,To,PidParent,self(),Stato#taxiListenerState.pidMoving},
	PidElezione = Stato#taxiListenerState.pidElection,
	gen_statem:cast(PidElezione, {partecipateElection,NewData}),
	{next_state, listen_election, Stato}.

%rimbalzo roba elezione a automa elettore
listen_election(cast, {election_data, Data}, Stato) ->
	PidElezione = Stato#taxiListenerState.pidElection,
	gen_statem:cast(PidElezione, Data),
	keep_stata_and_data.

%listen_election(cast, OtherEvents, Stato) ->
%	postpone


%% ====================================================================
%% Internal functions
%% ====================================================================


