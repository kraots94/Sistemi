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
-define(DEBUGPRINT_USER, false).

callback_mode() -> [state_functions].

-record(user, {
			   pidApp
  				}).

start(InitialPos, PID_GPS_Server) ->
	InitData = {InitialPos, PID_GPS_Server},
	{ok, Pid} = gen_statem:start_link(?MODULE,InitData, []),
	Pid.


init(InitData) ->
	{InitialPos, PidGpsServer} = InitData,
	PidApp = appUtente_flusso:start(InitialPos, PidGpsServer),
	State = #user{
				  pidApp = PidApp},
	{ok, idle, State}.

%env mi invia spostamento
idle(info, {moveToTarget, Request}, Stato) -> 
	appUtente_flusso:sendRequest(Stato#user.pidApp, Request),
	print_debug_message("Servizio disponibile!"),
	{nest_state,waitingService,Stato}.

waitingService(cast, {gotElectionData, Data}, Stato) ->
	TimeToWait = Data#election_result_to_user.time_to_wait,
	printDebug("Tempo d'attesa"),
	printDebug(TimeToWait),
	print_debug_message("Tempo d'attesa:"),
	{next_state, waiting_car,Stato}.
	
	



	

printDebug(ToPrint) ->
	if ?DEBUGPRINT_USER -> io:format("<user>"),
		  				  utilities:print_debug_message(self(), [?TILDE_CHAR] ++ "p", ToPrint);
	   true -> foo
	end.
	



