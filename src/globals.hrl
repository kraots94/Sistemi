-define(TILDE_CHAR, 126).

% paths for ECLIPSE
-define(FILE_MAP, "map/city_map_graph.dat").
-define(FILE_NODES, "map/city_map_nodes.dat").
-define(FILE_CHARGING_COLS, "map/city_map_charging_cols.dat").

% paths for terminal
%-define(FILE_MAP, "../map/city_map_graph.dat").
%-define(FILE_NODES, "../map/city_map_nodes.dat").
%-define(FILE_CHARGING_COLS, "../map/city_map_charging_cols.dat").

-define(HANDLE_COMMON,
    ?FUNCTION_NAME(T, C, D) -> handle_common(T, C,?FUNCTION_NAME, D)).

% Every Tick will last for TICKTIME seconds
-define(TICKTIME, 1000).

%numero di tick da ricevere per fare alc
-define(TICKS_TIMEOUT_ELECTION, 2).
-define(TICKS_EVENT, 20).

-define(GPS_MODULE_POWER, 50).
-define(MAP_SIDE, 100).

-define(BATTERY_LEVEL_LOW, 20).
-define(BATTERY_LEVEL_MAX, 100).