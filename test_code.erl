make:all(),
f(),
PID_GPS_SERVER = gps_server:start_gps_server(nodes_util:load_nodes()),
Mappa = city_map:init_city(),
Pid_car1 = macchina_ascoltatore:start({"s", PID_GPS_SERVER, Mappa}),
Pid_car2 = macchina_ascoltatore:start({"t", PID_GPS_SERVER, Mappa}),
PidUser = appUtente_flusso:start("r",PID_GPS_SERVER).

appUtente_flusso:sendRequest(PidUser, {"r","p"}).