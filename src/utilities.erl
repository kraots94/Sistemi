%% @author Alessandro
%% @doc @todo Add description to utilities.


-module(utilities).
-include("globals.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([println/1, 
		 println/2, 
		 printList/2,
		 print_message_raw/1,
		 print_debug_message/1,
		 print_debug_message/2,
		 print_debug_message/3, 
		 print_user_message/1,
		 print_user_message/2,
		 print_user_message/3, 
		 print_car_message/1,
		 print_car_message/2,
		 print_car_message/3, 
		 print_environment_message/1,
		 print_environment_message/2,
		 print_environment_message/3, 
		 print_message/4,
		 calculateSquaredDistance/2, 
		 generate_random_number/1, 
		 createPairsFromList/1]).

%print normal text
%print normal text
println(String) ->
	io:format(String ++ "~n", []).

%print text e variabile
println(String,Var) ->
	io:format(String ++ " ~p~n" , [Var]).

%print list
printList(String,List) -> 
	io:format(String ++ " ~w~n" , [List]).

print_message_raw(Text) -> io:format(Text).

print_debug_message(Message) -> 
	printMessage("Debug", "", Message, none).
print_debug_message(Format, Data) -> 
	fix_and_print_message("Debug", "", Format, Data).
print_debug_message(PID, Format, Data) -> 
	printMessage("Debug", PID, Format, Data).

print_user_message(Message) -> 
	printMessage("User", "", Message, none).
print_user_message(Format, Data) -> 
	fix_and_print_message("User", "", Format, Data).
print_user_message(PID, Format, Data) -> 
	printMessage("User", PID, Format, Data).

print_car_message(Message) ->
	printMessage("Car", "", Message, none).
print_car_message(Format, Data) -> 
	fix_and_print_message("Car", "", Format, Data).
print_car_message(PID, Format, Data) -> 
	printMessage("Car", PID, Format, Data).

print_environment_message(Message) -> 
	printMessage("Env", "", Message, none).
print_environment_message(Format, Data) -> 
	fix_and_print_message("Env", "", Format, Data).
print_environment_message(PID, Format, Data) -> 
	printMessage("Env", PID, Format, Data).

print_message(Type, PID, Format, Data) -> printMessage(Type, PID, Format, Data).

calculateSquaredDistance({Px, Py}, {Qx, Qy}) ->
	Diff_1 = Qx - Px,
	Diff_2 = Qy - Py,
	SquaredDistance = Diff_1 * Diff_1 + Diff_2 * Diff_2,
	SquaredDistance.

createPairsFromList(List) -> createPairs(List, []).

% Generates a number 1 <= x <= MAX
generate_random_number(MAX) ->
	N = rand:uniform(MAX),
% 	print_debug_message("", "Generated number: ~w ~n", N)
	N.

%% ====================================================================
%% Internal functions
%% ====================================================================
fix_and_print_message(Type, PID, Format, Data) -> 
	FormatToTest = if 
		is_list(Format) ->
			Format;
		true ->
			[Format]
	end,
	Tildes = countTilde(FormatToTest, 0),
	if 
		Tildes > 0 ->
			printMessage(Type, PID, Format, Data);
		true ->
			printMessage(Type, Format, Data, none)
	end.

printMessage(Type, PID, Format, Data) ->
	if 	PID == "" -> 
		   	if 
				Data == none -> 
					io:format("[~p] ~p ~n", [Type, Format]);
				true ->	
					if 
						is_list(Data) -> 
							Tildes = countTilde(Format, 0),
							if 
								Tildes > 1 ->
										io:format("[~p] - "++Format++"~n", [Type] ++ Data);
									true ->
										io:format("[~p] - "++Format++"~n", [Type, Data])
							end;
						true -> 
							io:format("[~p] - "++Format++"~n", [Type] ++ [Data])
					end
		   	end;
		true -> 
			if 
				Data == none -> 
					io:format("[~p] {~w} - ~p ~n", [Type, PID, Format]);
				true ->	
					if 
						is_list(Data) -> 
							Tildes = countTilde(Format, 0),
							if 
								Tildes > 1 ->
									io:format("[~p] {~w} - "++Format++"~n", [Type, PID] ++ Data);
								true ->
									io:format("[~p] {~w} - "++Format++"~n", [Type, PID, Data])
							end;
						true -> 
							io:format("[~p] {~w} - "++Format++"~n", [Type, PID] ++ [Data])
					end
			end	   
	end.

countTilde([], ACC) -> 
	ACC;
countTilde([H | T], ACC) -> 
	Res = if 
		H == ?TILDE_CHAR ->
			NextChar = hd(T),
			if 
				not (NextChar == "n") -> 
					1;
				true -> 
					0
			end;
		true -> 0
	end,
	NewAcc = ACC +Res,
	countTilde(T, NewAcc).

createPairs([], ACC) -> 
	ACC;

createPairs([A , B | []], ACC) -> 
	NEW_LIST = ACC ++ [{A, B}],
	NEW_LIST;

createPairs([A | []], _ACC) ->
	[{A,A}];

createPairs([A , B | Tail], ACC) ->
	NEW_LIST = ACC ++ [{A, B}],
	createPairs([B] ++ Tail, NEW_LIST).
