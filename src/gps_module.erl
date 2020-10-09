%% @author Alessandro
%% @doc @todo Add description to gps_module.

-module(gps_module).

-include("records.hrl").
-include("globals.hrl").

-import('send', [send_message/2, send_message/3]).
-import('utilities', [print_debug_message/1, print_debug_message/2, print_debug_message/3]).
-export([setPosition/2, deleteLocationTracks/1]).
-record(gpsModuleState, {pid_entity, pid_gps_server, entity_type, current_position, module_range, map_side}).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start_gps_module/1, init/1, end_gps_module/1]).

start_gps_module(StartingData) -> 
    spawn_link('gps_module', init, [StartingData]).

end_gps_module(Pid) ->
    print_debug_message("Killing GPS Module"),
	Ref = erlang:monitor(process, Pid),
	send_message(Pid, {self(), Ref, terminate}),
	
	receive
		{Ref, ok} ->
			erlang:demonitor(Ref, [flush]),
			ok;
		{'DOWN', Ref, process, Pid, Reason} ->
			erlang:error(Reason)
	after 5000 ->
		erlang:error(timeout)
	end.

setPosition(GpsPid, Position) ->
	send_message(GpsPid, {setPosition, Position}).

deleteLocationTracks(GpsPid) ->
	send_message(GpsPid, {removeEntity}).



%% ====================================================================
%% Init functions
%% ====================================================================

init(StartingData) -> 
    Type = StartingData#dataInitGPSModule.type,
    Pid_S = StartingData#dataInitGPSModule.pid_server_gps,
    Start_Pos = StartingData#dataInitGPSModule.starting_pos,
    Power = StartingData#dataInitGPSModule.signal_power,
    Pid_Entity = StartingData#dataInitGPSModule.pid_entity,
    Map_Side = StartingData#dataInitGPSModule.map_side,
	S = #gpsModuleState {
        pid_entity = Pid_Entity,
        pid_gps_server = Pid_S, 
        entity_type = Type, 
        current_position = Start_Pos, 
        module_range = Power,
        map_side = Map_Side
    },
    registerModule(S),
	loop(S).

registerModule(State) ->
    Type = State#gpsModuleState.entity_type,
    Pid_S = State#gpsModuleState.pid_gps_server,
    Start_Pos = State#gpsModuleState.current_position,
    Pid_Entity = State#gpsModuleState.pid_entity,
    send_message(Pid_S, Pid_Entity, {register, Type, Start_Pos}).
%% ====================================================================
%% Server loop
%% ====================================================================

loop(S) ->
	receive 
        {setPosition, NewPos}   ->  
                                Pid_S = S#gpsModuleState.pid_gps_server,
                                Pid_Entity = S#gpsModuleState.pid_entity,
                                NewState = S#gpsModuleState{current_position = NewPos},
                                send_message(Pid_S, Pid_Entity, {setPosition, NewPos}),
                                loop(NewState);
        {getPosition}           ->  
                                Pid_Entity = S#gpsModuleState.pid_entity,
                                send_message(Pid_Entity, S#gpsModuleState.current_position),
                                loop(S);
        {printState}            ->  
                                print_debug_message(self(), "GPS Module State: ~w", S),
                                loop(S);
        {removeEntity}          ->  % Calling this means to remove entry in gps server and kill this module instance
                                Pid_S = S#gpsModuleState.pid_gps_server,
                                Pid_Entity = S#gpsModuleState.pid_entity,
                                send_message(Pid_S, Pid_Entity, {removeEntity});
        {getNearestCar}         -> 
                                Pid_Entity = S#gpsModuleState.pid_entity,
                                Power = S#gpsModuleState.map_side * 1.4142,
                                Pid_S = S#gpsModuleState.pid_gps_server,
                                CurrentPos = S#gpsModuleState.current_position,
                                send_message(Pid_S, self(), {getSortedEntities, CurrentPos, Power}),
                                receive
                                    {_PID_S, Results} -> 
                                                FilteredResults = filterResultsByType(Results, car),
                                                MappedResults = mapResultsToPidsList(FilteredResults),
                                                ClearedResults = lists:delete(Pid_Entity, MappedResults),
                                                FirstCar = if ClearedResults =:= [] -> none;
                                                                    true -> hd(ClearedResults)
                                                            end,
                                                send_message(Pid_Entity, FirstCar);
                                    Unknown -> 
                                                print_debug_message(self(), "Bad data received: ~p", [Unknown])
                                end,
                                loop(S);
        {getNearCars}           -> 
                                Pid_Entity = S#gpsModuleState.pid_entity,
                                Power = S#gpsModuleState.module_range,
                                Pid_S = S#gpsModuleState.pid_gps_server,
                                CurrentPos = S#gpsModuleState.current_position,
                                send_message(Pid_S, self(), {getNearEntities, CurrentPos, Power}),
                                receive
                                    {_PID_S, Results} -> 
                                            FilteredResults = filterResultsByType(Results, car),
                                            MappedResults = mapResultsToPidsList(FilteredResults),
                                            ClearedResults = lists:delete(Pid_Entity, MappedResults),
                                            send_message(Pid_Entity, ClearedResults);
                                    Unknown -> 
                                            print_debug_message(self(), "Bad data received: ~p", [Unknown])
                                end,
                                loop(S);
        {PID, getNearCars}      -> 
                                Pid_Entity = S#gpsModuleState.pid_entity,
                                Power = S#gpsModuleState.module_range,
                                Pid_S = S#gpsModuleState.pid_gps_server,
                                CurrentPos = S#gpsModuleState.current_position,
                                send_message(Pid_S, self(), {getNearEntities, CurrentPos, Power}),
                                receive
                                    {_PID_S, Results} -> 
                                            FilteredResults = filterResultsByType(Results, car),
                                            MappedResults = mapResultsToPidsList(FilteredResults),
                                            ClearedResults = lists:delete(Pid_Entity, MappedResults),
                                            send_message(PID, ClearedResults);
                                    Unknown -> 
                                            print_debug_message(self(), "Bad data received: ~p", [Unknown])
                                end,
                                loop(S);
        {Pid, Ref, terminate}   ->
                                send_message(Pid, {Ref, ok}),
                                Pid_S = S#gpsModuleState.pid_gps_server,
                                Pid_Entity = S#gpsModuleState.pid_entity,
                                send_message(Pid_S, Pid_Entity, {removeEntity}),
                                print_debug_message(self(), "Exiting gps module loop", []);
        Unknown                 ->
                    			print_debug_message(self(), "Gps Module Received Unknown: ~p.", [Unknown]),
                                loop(S)
	end.

% Get List in form of [{PID, type}, ...]
filterResultsByType(Results, Type) ->
	FilterFunc = fun(EL) ->
			{_Pid_Ent, EL_Type} = EL,
			EL_Type == Type 
		end,
	FilteredResults = lists:filter(FilterFunc, Results),
	FilteredResults.

mapResultsToPidsList(Results) ->
	MapFunc = fun(EL) ->
			{Pid_Ent, _EL_Type} = EL,
			Pid_Ent
		end,
	MappedResults = lists:map(MapFunc, Results),
	MappedResults.
