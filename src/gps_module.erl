%% @author Alessandro
%% @doc @todo Add description to gps_module.


-module(gps_module).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Internal functions
%% ====================================================================



% Get List in form of [{PID, type}, ...]
filterResultsByType(Results, Type) ->
	FilterFunc = fun(EL) ->
			{_Pid_Ent, EL_Type} = EL,
			EL_Type == Type 
		end,
	FilteredResults = lists:filter(FilterFunc, Results),
	FilteredResults.

mapResultsToPidsList(Results) ->
	MapFunc = fun(EL) ->
			{Pid_Ent, _EL_Type} = EL,
			Pid_Ent
		end,
	MappedResults = lists:map(MapFunc, Results),
	MappedResults.
