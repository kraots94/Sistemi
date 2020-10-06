-module(macchina_batteria).
-compile(export_all).
-behaviour(gen_statem).
-include("globals.hrl").
-include("records.hrl").

callback_mode() -> [state_functions].

%!!!to do ricarica solare!!!
%nextToDo :
% 1) in macchina definisci enablePathCharge(accodi path colonnina) e fullBattery(torni in idle)
% 2) In macchina fai in modo che percorso verso colonnina venga salvato in attributo state e poi accodato alla ricezione enablePathCharge

%
%% ====================================================================
%% API functions
%% ====================================================================

start(AttachedCarPid) ->
	State = #batteryState {
					pidCar = AttachedCarPid,
					sameBatteryCounter = 0,
					tick_counter = 0,
					columnPathEnabled = false},
	{ok, Pid} = gen_statem:start_link(?MODULE,State, []),
	Pid.



%% ====================================================================
%% Automata Functions
%% ====================================================================

init(State) ->
	{ok, check_battery, State}.

check_battery(info, {_From, tick}, Stato) ->
	TickCount = Stato#batteryState.tick_counter,
	%for testi porp vedo a ogni tick
	if TickCount < 0 -> {keep_state, Stato#batteryState{tick_counter = TickCount + 1}}; 
		true ->
			PidAttachedCar = Stato#batteryState.pidCar,
			Battery = macchina_moving:getBatteryLevel(PidAttachedCar),
			NewState = check_threshold(Battery, Stato), %controllo < > min/max e same value from N ticks
			{keep_state, NewState}	
	end.

%% ====================================================================
%% Internal Functions
%% ====================================================================

%controlla i threshold e torna nuovo stato
check_threshold(BatteryLevel, Stato) ->
	PidAttachedCar = Stato#batteryState.pidCar,
	AlreadyEnabledColPath = Stato#batteryState.columnPathEnabled,
	NewState = if (BatteryLevel < 78) and not(AlreadyEnabledColPath) ->
		   			macchina_moving:enablePathCharge(PidAttachedCar), %macchina accoda tappe colonnina e poi va in stato ricarica...
		   			Stato#batteryState{columnPathEnabled = true};
				  (BatteryLevel == 100) and (AlreadyEnabledColPath) -> %(?) relazione sbagliata (?)
					macchina_moving:fullBattery(PidAttachedCar), %...e questo lo fa tornare in idle
					Stato#batteryState{columnPathEnabled = false};
				  true -> Stato
			   end,
	NewState#batteryState{tick_counter = 0}.
					
		   
		   