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

callback_mode() -> [state_functions, state_enter].

-record(userState, {
			   pidApp,
			   name,
			   pidEnv}).

%% ====================================================================
%% API functions
%% ====================================================================

receiveFromApp(PidUser, Data) ->
	gen_statem:cast(PidUser, Data).

sendRequest(PidUser, Request) ->
	gen_statem:cast(PidUser, {send_request,Request}).

die(UserPid) ->
	gen_statem:cast(UserPid, {die}).

dieSoft(UserPid) ->
	gen_statem:cast(UserPid, {dieSoft}).

%% ====================================================================
%% Automata Functions
%% ====================================================================
%InitData = {InitialPos: String, PidGpsServer, Name: String}
start(InitData) ->
	{ok, Pid} = gen_statem:start_link(?MODULE,InitData, []),
	Pid.

init(InitData) ->
	{InitialPos, PidGpsServer, Name, PidEnv} = InitData,
	InitDataApp = {InitialPos, PidGpsServer, self()},
	PidApp = appUtente:start(InitDataApp),
	State = #userState{pidApp = PidApp,
				  name = Name,
				  pidEnv = PidEnv},
	{ok, idle, State}.

handle_common(cast, {die}, _OldState, State) ->
	killEntities(State);

handle_common(cast, {dieSoft}, OldState, State) ->
	if OldState == idle ->
		killEntities(State);
	   true ->
		   keep_state_and_data
	end.

idle(enter, _OldState, _State) -> keep_state_and_data;

idle(cast, {send_request, Request}, State) ->
	{From, To} = Request,
	print_user_message(State#userState.name, "I'm a new user, i'm going from [~p] to [~p]", [From, To]),
	appUtente:sendRequest(State#userState.pidApp, Request),
	{next_state,waitingService,State};

?HANDLE_COMMON.


waitingService(enter, _OldState, _State) -> keep_state_and_data;
	
waitingService(cast, {gotElectionData, Data}, State) ->
	PID_Car = Data#election_result_to_user.id_car_winner,
	TimeToWait = Data#election_result_to_user.time_to_wait,
	print_user_message(State#userState.name, "Taxi ~w is serving me with ~w time to wait", [PID_Car, TimeToWait]),
	{next_state, waiting_car,State};


?HANDLE_COMMON.

waiting_car(enter, _OldState, _State) -> keep_state_and_data;

waiting_car(cast, {arrivedUserPosition, PID_Car}, State) ->
	print_user_message(State#userState.name, "Taxi ~w is arrived in my position!", PID_Car),
	{next_state, moving, State};

?HANDLE_COMMON.

moving(enter, _OldState, _State) -> keep_state_and_data;

moving(cast, {arrivedTargetPosition, Dest}, State) ->
	print_user_message(State#userState.name, "I'm arrived in my target position [~p]", Dest),
	{next_state, ending, State};

?HANDLE_COMMON.

ending(enter, _OldState, State) ->
	print_user_message(State#userState.name, "Mr. Stark, I don't feel so good."),
	environment:removeUser(State#userState.pidEnv, self()),
	killEntities(State);

?HANDLE_COMMON.
%% ====================================================================
%% Internal functions
%% ====================================================================

killEntities(State) ->
	PidAdd = State#userState.pidApp,
	gen_statem:cast(PidAdd, {die}),
	gen_statem:stop(self()).

printDebug(ToPrint) ->
	if ?DEBUGPRINT_USER -> io:format("<user>"),
		  				  	utilities:print_debug_message(self(), [?TILDE_CHAR] ++ "p", ToPrint);
	   true -> foo
	end.
