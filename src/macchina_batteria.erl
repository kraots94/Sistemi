-module(macchina_batteria).
-behaviour(gen_statem).
-include("globals.hrl").
-include("records.hrl").
-import('utilities', [print_debug_message/1, 
						print_debug_message/2, 
						print_debug_message/3,
						print_car_message/1,
						print_car_message/2,
						print_car_message/3]).

-export([start/1]).
-export([callback_mode/0, init/1, check_battery/3]).

callback_mode() -> [state_functions].
-define(TICKS_CHECK_BATTERY, 1).
-define(TICKS_TOGOSOLARCHARGE, 20).

-define(DEBUGPRINT_BATTERY, true).
-record(batteryState, {pidCar,				%pid macchina associata
					  sameBatteryCounter,   %contatore che conta per quanto tempo non c'è stato cambiamento alla batteria, per la ricarica solare
					  tick_counter,         %contatore generico tei tick
					  columnPathEnabled,    %flag che indica se è stato abilitato il path verso la colonnina al mezzo
					  notifiedChargedBat,   %flag che indica se è stato notificato il mezzo riguardo al termine della carica
					  nameCar}).			%il nome di questo mezzo

%
%% ====================================================================
%% API functions
%% ====================================================================

start({Pid_Moving, Name}) ->
	State = #batteryState {
					pidCar = Pid_Moving,
					nameCar = Name,
					sameBatteryCounter = 0,
					tick_counter = 0,
					columnPathEnabled = false,
					notifiedChargedBat = false
	},
	{ok, Pid} = gen_statem:start_link(?MODULE,State, []),
	Pid.

%% ====================================================================
%% Automata Functions
%% ====================================================================

init(State) ->
	printDebug("Init battery"),
	{ok, check_battery, State}.
			
handle_common(info, {_From, tick}, _OldState, State) ->
	PidAttachedCar = State#batteryState.pidCar,
	IsCarStationary = macchina_moving:areYouStationary(PidAttachedCar),
	
	if 
		IsCarStationary -> %è ferma in idle
			ActualTickStillStationary = State#batteryState.sameBatteryCounter + 1,
			BatteryLevel = macchina_moving:getBatteryLevel(PidAttachedCar),
			if 
				(ActualTickStillStationary >= ?TICKS_TOGOSOLARCHARGE) and (BatteryLevel < ?BATTERY_LEVEL_MAX) ->  
					print_car_message(State#batteryState.nameCar, "Battery - Triggered Solar Charging"),
					NewState = State#batteryState{sameBatteryCounter = 0},
					{keep_state, NewState, [{next_event,internal,activateSolarCharge}]};
				true -> 
					{keep_state, State#batteryState{sameBatteryCounter = ActualTickStillStationary}}
			end;
	  	true -> % è in moving oppure in carica, non è stazionaria in idle 
		   	ActualTickBatteryCheck = State#batteryState.tick_counter + 1,
			if 	
				ActualTickBatteryCheck >= ?TICKS_CHECK_BATTERY -> 
					NewState = State#batteryState{tick_counter = 0, sameBatteryCounter = 0},
					{keep_state, NewState, [{next_event,internal,checkThresholds}]}; 
				true -> 
					NewState = State#batteryState{tick_counter = ActualTickBatteryCheck},
					{keep_state, NewState}
			end
	 end.

%% ====================================================================
%% Internal Functions
%% ====================================================================

check_battery(internal, checkThresholds, Stato) ->
	PidAttachedCar = Stato#batteryState.pidCar,
	BatteryLevel = macchina_moving:getBatteryLevel(PidAttachedCar),
	DoingSolarCharge = macchina_moving:areYouDoingSolarCharge(PidAttachedCar) ,
	if 
		DoingSolarCharge -> %sta facendo ricarica solare
			if 
				BatteryLevel > ?BATTERY_LEVEL_MAX -> % smetti di ricaricare
					macchina_moving:fullBattery(PidAttachedCar);
				true -> 
					nothing_to_do
			end,
			keep_state_and_data;
		true -> %è in movimento, oppure in carica non solare
			AlreadyEnabledColPath = Stato#batteryState.columnPathEnabled,
			Notified = Stato#batteryState.notifiedChargedBat,
			NewState = if 
				(BatteryLevel < ?BATTERY_LEVEL_LOW) and not(AlreadyEnabledColPath) ->
					print_car_message(Stato#batteryState.nameCar, "Battery - Level under minimum value"),
					macchina_moving:enablePathCharge(PidAttachedCar), %macchina accoda tappe colonnina e poi va in stato ricarica...
					Stato#batteryState{columnPathEnabled = true};
				(BatteryLevel > ?BATTERY_LEVEL_LOW) and AlreadyEnabledColPath ->
					print_car_message(Stato#batteryState.nameCar, "Battery - Level over minimum"),
					Stato#batteryState{columnPathEnabled = false, notifiedChargedBat = false};
				(BatteryLevel > ?BATTERY_LEVEL_MAX) and not(Notified)-> 
					print_car_message(Stato#batteryState.nameCar, "Battery - Level maximum"),
					macchina_moving:fullBattery(PidAttachedCar), %...e questo lo fa tornare in idle
					Stato#batteryState{notifiedChargedBat = true};
				true -> Stato
			end,
	   		{keep_state, NewState}	
	end;

check_battery(internal, activateSolarCharge, Stato) ->
	PidAttachedCar = Stato#batteryState.pidCar,
	macchina_moving:activateSolarCharge(PidAttachedCar),
	keep_state_and_data;
	
?HANDLE_COMMON.
  
printDebug(ToPrint) ->
	if ?DEBUGPRINT_BATTERY -> utilities:print_debug_message(self(), [?TILDE_CHAR] ++ "p", ToPrint);
	   true -> foo
	end.