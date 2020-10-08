make:all(),
f(),
PID_GPS_SERVER = gps_server:start_gps_server(nodes_util:load_nodes()).
PIDS_CARS = utilities:createRandomEntities(PID_GPS_SERVER, 3),
PID_USER = appUtente_flusso:start("bi", PID_GPS_SERVER).

appUtente_flusso:sendRequest(PID_USER, {"bi","at"}).



make:all(),
f(),
PID_GPS_SERVER = gps_server:start_gps_server(nodes_util:load_nodes()),
Mappa = city_map:init_city(),
Pid_car1 = macchina_ascoltatore:start({"ax", PID_GPS_SERVER, Mappa}),
Pid_car2 = macchina_ascoltatore:start({"ay", PID_GPS_SERVER, Mappa}),
Pid_car3 = macchina_ascoltatore:start({"av", PID_GPS_SERVER, Mappa}),
PidUser = appUtente_flusso:start("aw",PID_GPS_SERVER).

appUtente_flusso:sendRequest(PidUser, {"aw","ah"}).
