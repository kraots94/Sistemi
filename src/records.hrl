-record(node, {name, id, pos_x, pos_y}).

% user_start, user_target, intermediate, column_path, column_end, none
-record(tappa, {user, type, t, node_name}).

-record(city, {total_nodes, total_edges, city_graph, nodes, column_positions}).

-record(user_request,{from, to}).
-record(election_result_to_car,{id_winner, id_app_user, request}).
-record(election_result_to_user,{id_car_winner, time_to_wait}).

% dati di inizializzazione del modulo gps
-record(dataInitGPSModule, {pid_entity, 
							name_entity,
							type, 
							pid_server_gps,
							starting_pos, 
							signal_power, 
							map_side}).

-record(dataElectionBegin,{
	request,
	pidAppUser}).

-record(dataElectionPartecipate, {
	pidParent,
	request, %la tupla {From,To}
	ttl}).