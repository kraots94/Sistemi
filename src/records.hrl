-record(node, {name, id, pos_x, pos_y}).

% user_start, user_target, intermediate, column_path, column_end, none
-record(tappa, {user, type, t, node_name}).

-record(city, {total_nodes, total_edges, city_graph, nodes}).

%stato macchina in movimento:
-record(movingCarState, {
				pidWirelessCard,
				tappe,
				currentUser = none,
				currentPos,
				batteryLevel}).

-record(userState, {pidEnv,
					pidWirelessCard,
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

-record(taxiListenerState, {pidMoving,
							pidBattery,
							pidElection}).

% dati di inizializzazione del modulo gps
-record(dataInitGPSModule, {pid_entity, 
							type, 
							pid_server_gps,
							starting_pos, 
							signal_power, 
							map_side}).