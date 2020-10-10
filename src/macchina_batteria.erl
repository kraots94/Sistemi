-module(macchina_batteria).
-compile(export_all).
-behaviour(gen_statem).
-include("globals.hrl").
-include("records.hrl").
-import('utilities', [print_debug_message/1, 
						print_debug_message/2, 
						print_debug_message/3,
						print_car_message/1,
						print_car_message/2,
						print_car_message/3]).

callback_mode() -> [state_functions].
-define(TICKS_CHECK_BATTERY, 1).
-define(TICKS_TOGOSOLARCHARGE, 7).

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
			
handle_common(info, {_From, tick}, _OldState, State) ->
	PidAttachedCar = State#batteryState.pidCar,
	IsCarStationary = macchina_moving:areYouStationary(PidAttachedCar),
	
	if IsCarStationary -> %è ferma in idle
		   ActualTickStillStationary = State#batteryState.sameBatteryCounter + 1,
		   BatteryLevel = macchina_moving:getBatteryLevel(PidAttachedCar),
		   if (ActualTickStillStationary >= ?TICKS_TOGOSOLARCHARGE) and (BatteryLevel < ?BATTERY_LEVEL_MAX) ->  {keep_state, State#batteryState{sameBatteryCounter = 0}, [{next_event,internal,activateSolarCharge}]};
			  true -> {keep_state, State#batteryState{sameBatteryCounter = ActualTickStillStationary}}
		   end;
	   true -> % è in moving oppure in carica, non è stazionaria in idle 
		   ActualTickBatteryCheck = State#batteryState.tick_counter + 1,
		   if ((ActualTickBatteryCheck >= ?TICKS_CHECK_BATTERY)) -> {keep_state, State#batteryState{tick_counter = 0, sameBatteryCounter = 0}, [{next_event,internal,checkThresholds}]}; 
				true -> {keep_state, State#batteryState{tick_counter = ActualTickBatteryCheck}}
		   end
	 end.
		

%% ====================================================================
%% Internal Functions
%% ====================================================================


check_battery(internal, checkThresholds, Stato) ->
	PidAttachedCar = Stato#batteryState.pidCar,
	BatteryLevel = macchina_moving:getBatteryLevel(PidAttachedCar),
	DoingSolarCharge = macchina_moving:areYouDoingSolarCharge(PidAttachedCar) ,
	if DoingSolarCharge -> %sta facendo ricarica solare
		if (BatteryLevel > ?BATTERY_LEVEL_MAX) -> 
			   macchina_moving:fullBattery(PidAttachedCar),
			   keep_state_and_data;
		   true -> keep_state_and_data
		end;
		true -> %è in movimento, oppure in carica non solare
			AlreadyEnabledColPath = Stato#batteryState.columnPathEnabled,
			Notified = Stato#batteryState.notifiedChargedBat,
			NewState = if (BatteryLevel < ?BATTERY_LEVEL_LOW) and not(AlreadyEnabledColPath) ->
		   					macchina_moving:enablePathCharge(PidAttachedCar), %macchina accoda tappe colonnina e poi va in stato ricarica...
		   					Stato#batteryState{columnPathEnabled = true};
				  			(BatteryLevel > ?BATTERY_LEVEL_LOW) and AlreadyEnabledColPath ->
							Stato#batteryState{columnPathEnabled = false, notifiedChargedBat = false};
				  			(BatteryLevel > ?BATTERY_LEVEL_MAX) and not(Notified)-> 
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
	
	
