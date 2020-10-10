%% @author Agnul
%% @doc @todo Add description to macchina_ascoltatore.

-module(macchina_ascoltatore).
-compile(export_all).
-behaviour(gen_statem).
-include("records.hrl").
-include("globals.hrl").
-import('utilities', [print_debug_message/1, 
						print_debug_message/2, 
						print_debug_message/3, 
						print_car_message/1,
						print_car_message/2,
						print_car_message/3, 
						generate_random_number/1]).

callback_mode() -> [state_functions].

-record(taxiListenerState, {pidMoving,
							pidBattery,
							pidElection,
							pidGps,
							pidClock,
							pidAppUser}).

-define(TICKS_TO_CHARGE, 3).


%% ====================================================================
%% API functions
%% ====================================================================

%si crea con {"nodo iniziale", PID_GPS_Server, mappa}
start(InitData) ->
	{ok, Pid} = gen_statem:start_link(?MODULE,InitData, []),
	Pid.

beginElection(Pid) ->
	gen_statem:call(Pid, {beginElectionUser}).

updatePosition(ListenerPid, Position) ->
	gen_statem:cast(ListenerPid, {updatePosition, Position}).

%already to use in other automata
sendToEsternalAutomata(ListenerdPid, Target, Data) ->
	gen_statem:cast(ListenerdPid, {to_outside, {Target, Data}}).

areYouKillable(Pid) ->
	gen_statem:call(Pid, {areYouKillable}).

dieSoft(ListenerPid) ->
	IsKillable = macchina_ascoltatore:areYouKillable(ListenerPid),
	if IsKillable ->
		   gen_statem:cast(ListenerPid, {die}),
		   killed;
	   true ->
		   not_killed
	end.

die(ListenerdPid) ->
	gen_statem:cast(ListenerdPid, {die}),
	killed.

%% ====================================================================
%% Automata functions
%% ====================================================================

init(InitData) -> 
	print_debug_message(self(), "Spawning Car with data: ~w", InitData),
	{InitialPos, PidGpsServer, City_Map} = InitData,
	%creo pid delle entita' associate
	PidGpsModule = gps_module:start_gps_module(initDataGpsModule(PidGpsServer,InitialPos)), %start gps module (and register)
	PidMoving = macchina_moving:start({InitialPos,self()}),
	PidBattery  = macchina_batteria:start(PidMoving),
	PidElection = macchina_elezione:start(self(),PidMoving,PidGpsModule,City_Map),
	PidClock = tick_server:start_clock([self(),PidMoving,PidBattery,PidElection]),
	State = #taxiListenerState {
					pidMoving   = PidMoving,
					pidBattery  = PidBattery,
					pidElection = PidElection,
					pidGps = PidGpsModule,
					pidClock = PidClock,
					pidAppUser = -1
			},
	print_car_message(self(), "Car ready in position [~p]", InitialPos),
	{ok, idle, State}.

handle_common({call,From}, {areYouKillable}, OldState, State) ->
	PidMoving =  State#taxiListenerState.pidMoving,
	IsMovingKillable = macchina_moving:areYouKillable(PidMoving),
	if (OldState == idle) and (IsMovingKillable) -> {next_state, idle, State, [{reply,From,true}]};
		true -> {next_state, OldState, State, [{reply,From,false}]}
	end;

handle_common(cast, {die}, _OldState, State) ->
	killEntities(State).

%roba che deve uscire
idle(cast, {to_outside, {Target, Data}}, _Stato) ->
	gen_statem:cast(Target, Data),
	keep_state_and_data;
	
%ricezione del tick
idle(info, {_From, tick}, _Stato) ->
	keep_state_and_data; %per ora non fare nulla

%macchina vuole aggiornare posizione
idle(cast, {updatePosition, CurrentPosition}, Stato) ->
	PidGps = Stato#taxiListenerState.pidGps,
	gps_module:setPosition(PidGps, CurrentPosition),
	keep_state_and_data;

%richiesta senza elezione (debugging porp.)
idle(cast, {send_requestNoElection, Request}, Stato) ->
	%{From,To,PidAppUser} = Request,
	gen_statem:cast(Stato#taxiListenerState.pidMoving, {requestRcv,Request}),
	keep_state_and_data;
	
%begin election ricevuto da app utente
%Data = {From,To,PidAppUser}
idle(cast, {beginElection, Data}, Stato) ->
	print_debug_message(self(), "Have to start election, received request: ~w", Data),
	{From,To,PidAppUser} = Data,
	Request = #user_request{from = From, to = To},
	NewData = #dataElectionBegin{request = Request, pidAppUser = PidAppUser},
	PidElezione = Stato#taxiListenerState.pidElection,
	gen_statem:cast(PidElezione, {beginElection,NewData}),
	S1 = Stato#taxiListenerState{pidAppUser = PidAppUser},
	{next_state, listen_election, S1};

%partecipate election ricevuto da altra macchina
idle(cast, {partecipateElection, Data}, Stato) ->
	print_debug_message(self(), "Request to partecipate election:  ~w", Data),
	PidElezione = Stato#taxiListenerState.pidElection,
	gen_statem:cast(PidElezione, {partecipateElection, Data}),
	{next_state, listen_election, Stato};

?HANDLE_COMMON.

%% ====================================================================
%% ELECTION functions
%% ====================================================================

listen_election(info, {_From, tick}, _Stato) ->
	keep_state_and_data; %per ora non fare nulla

%rimbalzo roba elezione a automa elettore
listen_election(cast, {election_data, Data}, Stato) ->	
	%print_debug_message(self(), "Listener - election_data - ~w", [Data]),
	PidElezione = Stato#taxiListenerState.pidElection,
	gen_statem:cast(PidElezione, Data),
	keep_state_and_data;

% data out of this car
listen_election(cast, {to_outside, {Target, Data}}, _Stato) ->
	gen_statem:cast(Target, Data),
	keep_state_and_data;

listen_election(cast, {beginElection, Data}, _Stato) ->
	{_From, _To, PidAppUser} = Data,
	gen_statem:cast(PidAppUser, {already_running_election_wait}),
	keep_state_and_data;

listen_election(cast, {partecipateElection, Data}, _Stato) ->
	PidSender = Data#dataElectionPartecipate.pidParent,
	gen_statem:cast(PidSender, {election_data, {invite_result, {self(), i_can_not_join}}}),
	keep_state_and_data;

listen_election(cast, {election_results, Data}, Stato) ->
	PidElezione = Stato#taxiListenerState.pidElection,
	{DataToUse, Is_Initiator} = if 
		is_list(Data) -> %meaning received by initiator
			% Tolgo l'elezione dallo stato di calcolo
			gen_statem:cast(PidElezione, {exit_final_state_initator}),
			{hd(Data), true};
		true -> 
			{Data, false}
	end,

	Pid_Car = DataToUse#election_result_to_car.id_winner,

	if 
		(Pid_Car == -1) and (Is_Initiator) -> % Avviso l'utente se non c'è un vincitore
			notify_user_no_taxi(Stato#taxiListenerState.pidAppUser);
		Pid_Car == self() -> 
			print_debug_message(self(), "I have won election"),
			%% Update coda moving
			gen_statem:call(PidElezione, {sendMovingQueue}),

			% notifica utente
			PidMoving = Stato#taxiListenerState.pidMoving,
			Pid_User = DataToUse#election_result_to_car.id_app_user,
			TimeToUser = macchina_moving:getTimeToUser(PidMoving),
			DataToUser = #election_result_to_user{
				id_car_winner = Pid_Car, 
				time_to_wait = TimeToUser
			},
			gen_statem:cast(Pid_User, {winner, DataToUser});
		true -> ok
	end,
	{next_state, idle, Stato};

?HANDLE_COMMON.


%listen_election(cast, OtherEvents, Stato) ->
%	postpone


%% ====================================================================
%% Internal functions
%% ====================================================================

initDataGpsModule(PidGpsServer,InitialPosition) ->
	#dataInitGPSModule{
		pid_entity = self(), 
		type = car, 
		pid_server_gps = PidGpsServer,
		starting_pos = InitialPosition, 
		signal_power = ?GPS_MODULE_POWER, 
		map_side = ?MAP_SIDE}.

notify_user_no_taxi(PID) ->
	Pid_User = PID,
	DataToUser = #election_result_to_user{
		id_car_winner = -1, 
		time_to_wait = -1
	},
	gen_statem:cast(Pid_User, {winner, DataToUser}).

killEntities(State) ->
	PidMov = State#taxiListenerState.pidMoving,
	PidBattery =  State#taxiListenerState.pidBattery,
	PidElection = State#taxiListenerState.pidElection,
	PidGps = State#taxiListenerState.pidGps,
	PidClock = State#taxiListenerState.pidClock,
	gen_statem:stop(PidMov),gen_statem:stop(PidBattery),gen_statem:stop(PidElection), %ammazzo automi
	gps_module:end_gps_module(PidGps), tick_server:end_clock(PidClock), %ammazzo server
	print_debug_message(self(), "All linked entities killed"),
	gen_statem:stop(self()).