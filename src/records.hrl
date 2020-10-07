-record(node, {name, id, pos_x, pos_y}).

% user_start, user_target, intermediate, column_path, column_end, none
-record(tappa, {user, type, t, node_name}).

-record(city, {total_nodes, total_edges, city_graph, nodes}).

-record(user_request,{from, to}).
-record(election_result_to_car,{id_winner, id_app_user}).
-record(election_result_to_user,{id_car_winner, time_to_wait}).

-record(userState, {pidEnv,
					pidGPSModule,
					currentPos,
					currentDestination
					}).
% stato environment:
-record(environmentState, {cars, 
							users, 
							city, 
							tick_s_pid, 
							wireless_card_server_pid, 
							tick_counter}).

% dati di inizializzazione del modulo gps
-record(dataInitGPSModule, {pid_entity, 
							type, 
							pid_server_gps,
							starting_pos, 
							signal_power, 
							map_side}).

-record(batteryState, {pidCar,
					  sameBatteryCounter,
					  tick_counter,
					  columnPathEnabled}).

-record(dataElectionBegin,{
	request,
	pidAppUser}).

-record(dataElectionPartecipate, {
	pidParent,
	request, %la tupla {From,To}
	ttl}).