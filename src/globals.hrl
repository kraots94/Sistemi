
-define(FILE_MAP, "map\\city_map_graph.dat").
-define(FILE_NODES, "map\\city_map_nodes.dat").
-define(HANDLE_COMMON,
    ?FUNCTION_NAME(T, C, D) -> handle_common(T, C,?FUNCTION_NAME, D)).
% Every Tick will last for TICKTIME seconds
-define(TICKTIME, 1).

%numero di tick da ricevere per fare alc
-define(TICKS_TO_MOVING, 10).
-define(TICKS_TIMEOUT, 2).
-define(TICKS_EVENT, 20).

