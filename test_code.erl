make:all(),
f(),
PID_GPS_SERVER = gps_server:start_gps_server(nodes_util:load_nodes()).
PIDS_CARS = my_util:createRandomEntities(PID_GPS_SERVER, 1),
PID_USER = appUtente_flusso:start("bi",0, PID_GPS_SERVER).
appUtente_flusso:sendRequest(PID_USER, {"bi","at"}).