%quuesto file contiene definizioni di record generali, ossia conosciuti da almeno 2 moduli distinti

%nodo interno alla mappa  
-record(node, {name, id, pos_x, pos_y}).

%rappresenta una generica tappa del veicolo, ossia uno spostamento fra un nodo e un altro della città
%user :: Pid, il pid dell'app dell'utente che sto servendo in questa tappa
%type :: atomo, può assumere diversi valori a seconda del tipo della tappa : 'user_start', 'user_target', 'intermediate', 'column'
%t :: Digit, tempo per percorrere questa tappa
%node_name :: String, il nome del nodo che verrà raggiunto dalla tappa
-record(tappa, {user, type, t, node_name}).

%rappresentazione della città
-record(city, {total_nodes, total_edges, city_graph, nodes, column_positions}).

% dati di inizializzazione del modulo gps
-record(dataInitGPSModule, {pid_entity, 
							name_entity,
							type, 
							pid_server_gps,
							starting_pos, 
							signal_power, 
							map_side}).

%i record che riguardano l'elezione
-record(election_result_to_user,{id_car_winner, time_to_wait, name_car}).
-record(election_result_to_car,{id_winner, id_app_user, request}).
-record(user_request,{from, to}).

-record(dataElectionBegin,{
	request,
	pidAppUser}).

-record(dataElectionPartecipate, {
	pidParent,
	request, 
	ttl}).