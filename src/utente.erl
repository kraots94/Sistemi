-module(utente).
-compile(export_all).
-behaviour(gen_statem).
-import('send', [send_message/2, send_message/3]).
-import('utilities', [println/1, println/2, 
						print_debug_message/1, 
						print_debug_message/2, 
						print_debug_message/3,
						print_user_message/1,
						print_user_message/2,
						print_user_message/3]).
-include("globals.hrl").
-include("records.hrl").
-define(DEBUGPRINT_USER, true).

callback_mode() -> [state_functions].

-record(user, {
			   pidApp
  				}).

%% ====================================================================
%% API functions
%% ====================================================================

%nelle api metti il die che ammazza anche app

receiveFromApp(PidUser, Data) ->
	gen_statem:cast(PidUser, Data).

sendRequest (PidUser, Request) ->
	gen_statem:cast(PidUser, {send_request,Request}).

die(UserPid) ->
	gen_statem:cast(UserPid, {die}).

dieSoft(UserPid) ->
	gen_statem:cast(UserPid, {dieSoft}).

%% ====================================================================
%% Automata Functions
%% ====================================================================


%InitData = {InitialPos, PID_GPS_Server},
start(InitData) ->
	{ok, Pid} = gen_statem:start_link(?MODULE,InitData, []),
	Pid.


init(InitData) ->
	{InitialPos, PidGpsServer} = InitData,
	InitDataApp = {InitialPos, PidGpsServer, self()},
	PidApp = appUtente_flusso:start(InitDataApp),
	State = #user{
				  pidApp = PidApp},
	{ok, idle, State}.

handle_common(cast, {die}, _OldState, State) ->
	killEntities(State);

handle_common(cast, {dieSoft}, OldState, State) ->
	if OldState == idle ->
		killEntities(State);
	   true ->
		   keep_state_and_data
	end.
       
idle(cast, {send_request, Request}, State) ->
	appUtente_flusso:sendRequest(State#user.pidApp, Request),
	{next_state,waitingService,State};

?HANDLE_COMMON.
	
waitingService(cast, {gotElectionData, Data}, State) ->
	TimeToWait = Data#election_result_to_user.time_to_wait,
	printDebug("Servizio disponibile!"),
	printDebug("Tempo d'attesa:"),
	printDebug(TimeToWait),
	{next_state, waiting_car,State};

?HANDLE_COMMON.

waiting_car(cast, arrivedUserPosition, State) ->
	printDebug("taxi arrivato da me!"),
	{next_state, moving, State};

?HANDLE_COMMON.

moving(cast, arrivedTargetPosition, _State) ->
	printDebug("raggiunto destinazione finale!"),
	printDebug("me ne vado..."),
	keep_state_and_data;

?HANDLE_COMMON.
	
%% ====================================================================
%% Internal functions
%% ====================================================================

killEntities(State) ->
	PidAdd = State#user.pidApp,
	gen_statem:cast(PidAdd, {die}),
	gen_statem:stop(self()).

printDebug(ToPrint) ->
	if ?DEBUGPRINT_USER -> io:format("<user>"),
		  				  utilities:print_debug_message(self(), [?TILDE_CHAR] ++ "p", ToPrint);
	   true -> foo
	end.


	



