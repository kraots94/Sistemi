-module(macchina_batteria).
-compile(export_all).
-behaviour(gen_statem).
-include("globals.hrl").
-include("records.hrl").
-import('utilities', [print_debug_message/1, print_debug_message/2, print_debug_message/3]).

callback_mode() -> [state_functions].
-define(TICKS_CHECK_BATTERY, 1).

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
					columnPathEnabled = false,
					notifiedChargedBat = false},
	{ok, Pid} = gen_statem:start_link(?MODULE,State, []),
	Pid.



%% ====================================================================
%% Automata Functions
%% ====================================================================

init(State) ->
	{ok, check_battery, State}.

check_battery(info, {_From, tick}, State) ->
	ActualTickCounter = State#batteryState.tick_counter,
	NewCounter = ActualTickCounter + 1,
	%for testi porp vedo a ogni tick
	if NewCounter >= ?TICKS_CHECK_BATTERY -> {keep_state, State#batteryState{tick_counter = 0}, [{next_event,internal,checkThresholds}]};
		true ->
			{keep_state, State#batteryState{tick_counter = NewCounter}}
	end;

%% ====================================================================
%% Internal Functions
%% ====================================================================


check_battery(internal, checkThresholds, Stato) ->
	PidAttachedCar = Stato#batteryState.pidCar,
	BatteryLevel = macchina_moving:getBatteryLevel(PidAttachedCar),
	AlreadyEnabledColPath = Stato#batteryState.columnPathEnabled,
	Notified = Stato#batteryState.notifiedChargedBat,
	NewState = if (BatteryLevel < ?BATTERY_LEVEL_LOW) and not(AlreadyEnabledColPath) ->
		   			macchina_moving:enablePathCharge(PidAttachedCar), %macchina accoda tappe colonnina e poi va in stato ricarica...
		   			Stato#batteryState{columnPathEnabled = true};
				  (BatteryLevel > ?BATTERY_LEVEL_LOW) and AlreadyEnabledColPath ->
					Stato#batteryState{columnPathEnabled = false, notifiedChargedBat = false};
				  (BatteryLevel >= ?BATTERY_LEVEL_MAX) and not(Notified)-> 
					macchina_moving:fullBattery(PidAttachedCar), %...e questo lo fa tornare in idle
					Stato#batteryState{notifiedChargedBat = true};
				  true -> Stato
			   end,
	{keep_state, NewState}.
	
	
