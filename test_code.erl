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



%semplice spostamento, vince PidCar2
make:all(),
f(),
PID_GPS_SERVER = gps_server:start_gps_server(nodes_util:load_nodes()),
Mappa = city_map:init_city(),
PidCar1 = macchina_ascoltatore:start({"as",PID_GPS_SERVER, Mappa, "C1"}),
PidCar2 = macchina_ascoltatore:start({"ax",PID_GPS_SERVER, Mappa, "C2"}),
PidCar3 = macchina_ascoltatore:start({"ac",PID_GPS_SERVER, Mappa, "C3"}),
PidUser1 = utente:start({"aq", PID_GPS_SERVER, "U1"}).

utente:sendRequest(PidUser1, {"aq", "au"}).

%qua invii richiesta di U2 quando U1 arriva a dest (poco prima), C2 vince e accoda il suo path 
%se invece mandi tutte e due richieste assieme vengono gestite da 2 taxi diversi

%puoi far vedere anche ricariche alle colonnine, inserendo la terza dopo le pime due fai vedere spostamento 
%auto e poi spostamento verso colonnina
make:all(),
f(),
PID_GPS_SERVER = gps_server:start_gps_server(nodes_util:load_nodes()),
Mappa = city_map:init_city(),
PidCar1 = macchina_ascoltatore:start({"as",PID_GPS_SERVER, Mappa, "C1"}),
PidCar2 = macchina_ascoltatore:start({"ax",PID_GPS_SERVER, Mappa, "C2"}),
PidCar3 = macchina_ascoltatore:start({"ac",PID_GPS_SERVER, Mappa, "C3"}),
PidUser1 = utente:start({"aq", PID_GPS_SERVER, "U1", 0}),
PidUser2 = utente:start({"au", PID_GPS_SERVER, "U2", 0}),
utente:sendRequest(PidUser1, {"aq", "au"}),

utente:sendRequest(PidUser2, {"au", "ba"}).

PidUser3 = utente:start({"az", PID_GPS_SERVER, "U4", 0}),


utente:sendRequest(PidUser3, {"az","aa"}).

%cambio dest, metto richiesta lontana dal nodo partenza
make:all(),
f(),
PID_GPS_SERVER = gps_server:start_gps_server(nodes_util:load_nodes()),
Mappa = city_map:init_city(),
PidCar1 = macchina_ascoltatore:start({"as",PID_GPS_SERVER, Mappa, "C1"}),
PidUser1 = utente:start({"aq", PID_GPS_SERVER, "U1", 0}),
utente:sendRequest(PidUser1, {"aq", "ay"}),

utente:changeDestination(PidUser1, "az").




make:all(),
f(),
PID_GPS_SERVER = gps_server:start_gps_server(nodes_util:load_nodes()),
Mappa = city_map:init_city(),
PidCar1 = macchina_ascoltatore:start({"as",PID_GPS_SERVER, Mappa, "C1"}),
PidUser1 = utente:start({"aq", PID_GPS_SERVER, "U1", 0}),
PidUser2 = utente:start({"au", PID_GPS_SERVER, "U2", 0}).

utente:sendRequest(PidUser1, {"aq", "au"}),

utente:sendRequest(PidUser2, {"au", "ba"}).

utente:changeDestination(PidUser1, "az").