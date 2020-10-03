-module(wireless_card).
-compile(export_all).

-record(entityPosition, {pid, position}). 
-record(wirelessCardState, {entityPositions, %lista di entityPosition ossia di roba {Pid, {x,y}}
				nodiMappa}).

start_link() -> spawn_link(fun init/0).

init() -> 
	NodiMappa = nodes_util:load_nodes(),
	S = #wirelessCardState{entityPositions = [], nodiMappa = NodiMappa},
	loop(S).

%metti invio msg al pid
loop(S) ->
	receive 
		{Pid, {setPosition, Node}} -> Position = nodes_util:getPositionFromNodeName(Node, S#wirelessCardState.nodiMappa),
									  NewPositions = setNewPos(S#wirelessCardState.entityPositions, Position, Pid),
									  loop(S#wirelessCardState{entityPositions = NewPositions});
		{Pid, {getPosition}}       -> Pid ! getPos(S#wirelessCardState.entityPositions, Pid);
		{Pid, {getNear, Copertura}} -> Pid ! getNear(Pid, Copertura, S);
		{Pid, {getNearest, Copertura}} -> Pid ! hd(getNear(Pid, Copertura,S)) %se fai sorting del risultato di getNear sulla distanza puoi fare cosi
	end.

getNear(Pid, Copertura, S) ->
	PosizionePid = getPos(S#wirelessCardState.entityPositions, Pid),
	lists:filter(fun(P) -> PuntoInEsame = P#entityPosition.position,
						   D = distanzaFraDuePunti(PosizionePid, PuntoInEsame),
						   (D < Copertura and (PuntoInEsame /= PosizionePid)) end, S#wirelessCardState.entityPositions).
%metti la formula qua
distanzaFraDuePunti({X1,Y1}, {X2,Y2}) ->
	X1 + X2.

%%da fare in modo sensato aggiornando valore se c'è e accodandolo se non c'è
setNewPos(Posizioni, Posizione, Pid) ->
	NuovoRecord = #entityPosition{pid = Pid, position = Posizione},
	Posizioni ++ [NuovoRecord].

%test(F) ->
%	Res = hd(lists:filter(fun(R) -> (R#entityPosition.pid == F) end, [R1,R2])).
	
getPos(Posizioni, ToSearch) ->
	hd(lists:filter(fun(X) -> (X#entityPosition.pid == ToSearch) end, Posizioni)).

	
	
