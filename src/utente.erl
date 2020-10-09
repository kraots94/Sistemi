-module(utente).
-compile(export_all).
-behaviour(gen_statem).
-import('send', [send_message/2, send_message/3]).
-import('utilities', [println/1, println/2, prinprint_debug_message/1, print_debug_message/2, print_debug_message/3]).
-include("globals.hrl").
-include("records.hrl").
-define(DEBUGPRINT_UTENTE, false).

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
%idle(info, {moveToTarget, Request}, Stato) -> 
%	sendRequest(Stato#user.pidApp, Request),
%	{nest_state,waitingService,Stato}.

	


	



