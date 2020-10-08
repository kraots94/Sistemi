make:all(),
f(),
PID_GPS_SERVER = gps_server:start_gps_server(nodes_util:load_nodes()).

PIDS_CARS = utilities:createRandomEntities(PID_GPS_SERVER, 3),
PID_USER = appUtente_flusso:start("bi", PID_GPS_SERVER, "").
appUtente_flusso:start({InitialPos, PidGpsServer, City_Map})
appUtente_flusso:sendRequest(PID_USER, {"bi","at"}).