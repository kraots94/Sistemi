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

start(InitialPos, PidWireless) ->
	PidMoving = macchina_moving_withRecords:start(InitialPos, PidWireless),
	my_util:println("Pid moving creato:", PidMoving),
	%PidElection = ...
	%PidBattery  = ...
	State = #taxiListenerState {
					pidMoving = PidMoving,
					pidBattery = none},
	{ok, Pid} = gen_statem:start_link(?MODULE,State, []),
	Pid.

beginElection(Pid) ->
	gen_statem:call(Pid, {beginElectionUser}).

%% ====================================================================
%% Automata functions
%% ====================================================================

init(State) -> 
	PidMoving = State#taxiListenerState.pidMoving,
	%PidElection = ...
	%PidBattery  = ...
	tick_server:start_clock(?TICKTIME, [self(),PidMoving]), %+ tutti gli altri...
	{ok, idle, State}.
	
%V0 features 
idle({call,From}, {beginElectionUser}, State) ->
	PidMoving = State#taxiListenerState.pidMoving,
	_PidElection = State#taxiListenerState.pidElection,
	IsBusy = macchina_moving_withRecords:areYouBusy(PidMoving),
	if IsBusy -> 
		   		%invii sto messaggio a PidElection (usando suo metodo api)
				%dicendo che non puoi vincere (bridge mode)
				foo;
	   true ->
		   %invii sto messaggio a PidElection (usando suo metodo api)
				%dicendo che puoi vincere (bridge mode)
				foo2
	end,
	{next_state, listen_election, State, [{reply,From,yes}]};

%come sopra sostanzialmente solo che devi dire a automa elezione che sei partecipating non initiating
idle(cast, {partecipateElection}, State) ->
	PidElection = State#taxiListenerState.pidElection,
	%invii sto messaggio a PidElection (usando suo metodo api)
	keep_stata_and_data.

%% ====================================================================
%% Internal functions
%% ====================================================================


