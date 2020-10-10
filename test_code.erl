make:all(),
f(),
PID_GPS_SERVER = gps_server:start_gps_server(nodes_util:load_nodes()),
Mappa = city_map:init_city(),
Pid_car1 = macchina_ascoltatore:start({"s", PID_GPS_SERVER, Mappa}),
Pid_car2 = macchina_ascoltatore:start({"t", PID_GPS_SERVER, Mappa}),
PidUser = appUtente:start("r",PID_GPS_SERVER).
appUtente:sendRequest(PidUser, {"r","p"}).



make:all(),
f(),
PID_GPS_SERVER = gps_server:start_gps_server(nodes_util:load_nodes()),
Mappa = city_map:init_city(),
Pid_car1 = macchina_ascoltatore:start({"h", PID_GPS_SERVER, Mappa}),
Pid_car2 = macchina_ascoltatore:start({"g", PID_GPS_SERVER, Mappa}),
Pid_car3 =  macchina_ascoltatore:start({"e",  PID_GPS_SERVER, Mappa}),

PidUser = appUtente:start("c",PID_GPS_SERVER),
PidUser2 = appUtente:start("p",PID_GPS_SERVER),
PidUser3 = appUtente:start("a", PID_GPS_SERVER),
appUtente:sendRequest(PidUser, {"c","p"}).

appUtente:sendRequest(PidUser2, {"p","j"}).

appUtente:sendRequest(PidUser3, {"a","s"}).

make:all(),
f(),
PID_GPS_SERVER = gps_server:start_gps_server(nodes_util:load_nodes()),
Mappa = city_map:init_city(),
Pid_car1 = macchina_ascoltatore:start({"s", PID_GPS_SERVER, Mappa}),
Pid_car2 = macchina_ascoltatore:start({"t", PID_GPS_SERVER, Mappa}),
PidUser = utente:start({"r",PID_GPS_SERVER}).
utente:sendRequest(PidUser, {"r","p"}).

make:all(),
f(),
PID_GPS_SERVER = gps_server:start_gps_server(nodes_util:load_nodes()),
Mappa = city_map:init_city(),
Pid_car1 = macchina_ascoltatore:start({"s", PID_GPS_SERVER, Mappa}).

macchina_ascoltatore:dieSoft(Pid_car1).

