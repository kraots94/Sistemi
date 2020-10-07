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

%si crea con {"nodo iniziale", PID_GPS_Server, mappa}
start(InitData) ->
	{ok, Pid} = gen_statem:start_link(?MODULE,InitData, []),
	Pid.

beginElection(Pid) ->
	gen_statem:call(Pid, {beginElectionUser}).

%% ====================================================================
%% Automata functions
%% ====================================================================

init(InitData) -> 
	{InitialPos, PID_GPS_Server, City_Map} = InitData,
	StartingDataGps = #dataInitGPSModule{
							pid_entity = self(), 
							type = car, 
							pid_server_gps = PID_GPS_Server,
							starting_pos = InitialPos, 
							signal_power = ?GPS_MODULE_POWER, 
							map_side = ?MAP_SIDE},
	PidGpsModule = gps_module:start_gps_module(StartingDataGps),
	PidMoving = macchina_moving:start(InitialPos, PidGpsModule),
	PidBattery  = macchina_batteria:start(PidMoving),
	PidElection = macchina_elezione:start(self(),PidMoving,PidGpsModule, City_Map),
	PidClock = tick_server:start_clock([self(),PidMoving,PidBattery,PidElection]),
	State = #taxiListenerState {
					pidMoving   = PidMoving,
					pidBattery  = PidBattery,
					pidElection = PidElection,
					pidClock = PidClock
			},
	%my_util:printList("Pid creati:", [PidMoving, PidBattery, PidElection]),
	{ok, idle, State}.
	
idle(info, {_From, tick}, _Stato) ->
	keep_state_and_data; %per ora non fare nulla
	
%begin election ricevuto da app utente
%Data = {From,To,PidAppUser}
idle(cast, {beginElection, Data}, Stato) ->
	{From,To,PidAppUser} = Data,
	Request = #user_request{from = From, to = To},
	NewData = #dataElectionBegin{request = Request, pidAppUser = PidAppUser},
	PidElezione = Stato#taxiListenerState.pidElection,
	gen_statem:cast(PidElezione, {beginElection,NewData}),
	{next_state, listen_election, Stato};

%partecipate election ricevuto da altra macchina
idle(cast, {partecipateElection, Data}, Stato) ->
	PidElezione = Stato#taxiListenerState.pidElection,
	gen_statem:cast(PidElezione, {partecipateElection, Data}),
	{next_state, listen_election, Stato}.

%% ====================================================================
%% ELECTION functions
%% ====================================================================

listen_election(info, {_From, tick}, _Stato) ->
	keep_state_and_data; %per ora non fare nulla

%rimbalzo roba elezione a automa elettore
listen_election(cast, {election_data, Data}, Stato) ->
	PidElezione = Stato#taxiListenerState.pidElection,
	gen_statem:cast(PidElezione, Data),
	keep_state_and_data;

listen_election(cast, {to_outside, {Target, Data}}, _Stato) ->
	gen_statem:cast(Target, Data),
	keep_state_and_data;

listen_election(cast, {beginElection, Data}, _Stato) ->
	{PID_APP_USER, _Request} = Data,
	gen_statem:cast(PID_APP_USER, {already_running_election_wait}),
	keep_state_and_data;

listen_election(cast, {partecipateElection, Data}, _Stato) ->
	PidSender = Data#dataElectionPartecipate.pidParent,
	gen_statem:cast(PidSender, {election_data, {invite_result, {self(), i_can_not_join}}}),
	keep_state_and_data;

listen_election(cast, {election_results, Data}, Stato) ->
	% notifico all'utente 
	io:format("ASCOLTATORE - Winning Results: ~w~n", [Data]),
	{next_state, idle, Stato}.


%listen_election(cast, OtherEvents, Stato) ->
%	postpone


%% ====================================================================
%% Internal functions
%% ====================================================================


