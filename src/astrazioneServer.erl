-module(astrazioneServer).
-export([start/2, start_link/2, call/2, cast/2, reply/2]).

%simile al gen_server delle librerie erlang.

%%% Public API
start(Module, InitialState) ->
    spawn(fun() -> init(Module, InitialState) end).

start_link(Module, InitialState) ->
    spawn_link(fun() -> init(Module, InitialState) end).

%astrazione di chiamata sincrona invio @param Msg al server con pid @param Pid(mi aspetto reply)
%creazione monitor per controllare crash server
call(Pid, Msg) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {sync, self(), Ref, Msg},
    receive
        {Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)
    after 5000 ->
        erlang:error(timeout)
    end.

%astrazione chiamata asincrona invio @param Msg (non aspetto reply)
cast(Pid, Msg) ->
    Pid ! {async, Msg},
    ok.

%reply al client con pid @param Pid che identifica questo msg con red @param Ref
reply({Pid, Ref}, Reply) ->
    Pid ! {Ref, Reply}.

%%% Private stuff
init(Module, InitialState) ->
    loop(Module, Module:init(InitialState)).

loop(Module, State) ->
    receive
        {async, Msg} ->
             loop(Module, Module:handle_cast(Msg, State));
        {sync, Pid, Ref, Msg} ->
             loop(Module, Module:handle_call(Msg, {Pid, Ref}, State))
    end.

%esempi utilizzo, codice server specifico (da mettere in altro modulo obv): 

%API del client, che chiama queste
%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
	my_server:call(Pid, {order, Name, Color, Description}).
 
%% This call is asynchronous
return_cat(Pid, Cat = #cat{}) ->
	my_server:cast(Pid, {return, Cat}).
 
%% Synchronous call
close_shop(Pid) ->
	my_server:call(Pid, terminate).

%funzioni del server che "ampliano" il loop:

handle_call({order, Name, Color, Description}, From, Cats) ->
	if Cats =:= [] ->
		my_server:reply(From, make_cat(Name, Color, Description)), %nel gen_server questo è regalato (chiamata a reply)
		Cats;
	   Cats =/= [] ->
		my_server:reply(From, hd(Cats)),
		tl(Cats)
	end;
 
handle_call(terminate, From, Cats) ->
	my_server:reply(From, ok),
	terminate(Cats).
 
handle_cast({return, Cat = #cat{}}, Cats) ->
	[Cat|Cats].
