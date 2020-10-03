-record(node, {name, id, pos_x, pos_y}).
% user_start, user_target, intermediate, column_path, column_end, none
-record(tappa, {user, type, t, node_name}).
-record(city, {total_nodes, total_edges, city_graph, nodes}).
%stato macchina in movimento:
-record(movingCarState, {tappe,
				currentUser = none,
				currentPos}).
% stato environment:
-record(environmentState, {cars, users, city, tick_s_pid}).
