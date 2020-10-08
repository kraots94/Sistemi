-define(TILDE_CHAR, 126).

-define(FILE_MAP, "map\\city_map_graph.dat").
-define(FILE_NODES, "map\\city_map_nodes.dat").

-define(HANDLE_COMMON,
    ?FUNCTION_NAME(T, C, D) -> handle_common(T, C,?FUNCTION_NAME, D)).

% Every Tick will last for TICKTIME seconds
-define(TICKTIME, 1).

%numero di tick da ricevere per fare alc
-define(TICKS_TO_MOVING, 2).
-define(TICKS_TIMEOUT_ELECTION, 2).
-define(TICKS_EVENT, 100).
-define(TICKS_CHECK_BATTERY, 1).

-define(GPS_MODULE_POWER, 50).
-define(MAP_SIDE, 100).

-define(MAX_HOPES_ELECTION, 2).

-define(BATTERY_LEVEL_LOW, 20).
-define(BATTERY_LEVEL_MAX, 100).