%% ITESM CEM, May 10, 2010.
%% Erlang Source File
%% Activity: Collector
%% Author: Artruro Nereu Nunez Martinez, 1163145
%% Author: Diego Arturo Gurrusquieta Quezada, 1163085
%% Author: Juan Manuel Fernandez Perez 1162937

-module(server).
-compile(export_all).

-define(DECK, lists:flatten(lists:duplicate(4, [a,2,3,4,5,6,7,8,9,10,j,q,k]))).
-define(TIME_OUT, 10 * 60 * 1000). % Ten Minutes
%-------------------------------------------------------------------------------
start() ->
	register(server, spawn(fun() -> process_flag(trap_exit, true),
    main_loop(dict:new()) end)),
    ok.
%-------------------------------------------------------------------------------
stop() ->
    server ! stop,
    ok.
%-------------------------------------------------------------------------------
test() -> ok.
%-------------------------------------------------------------------------------
create(Name, NumPlayers) ->
    server ! {create, self(), Name, NumPlayers},
    rcv().
%-------------------------------------------------------------------------------
available() ->
    server ! {available, self()},
    rcv().
%-------------------------------------------------------------------------------
join(Name) ->
    server ! {join, self(), Name},
    rcv().
%-------------------------------------------------------------------------------
rcv() ->
    receive
        {collector, Result} -> Result
    end.
%-------------------------------------------------------------------------------
main_loop(Dict) ->
    	receive
        		{create, From, Name, NumPlayers} -> create_game(Dict, From, Name, NumPlayers);
				{available, From} -> available_games(Dict, From);
				{join, From, Name} -> join_game(Dict, From, Name);
				{'EXIT', Pid, _} -> main_loop(remove_dead_process(Pid, Dict));
				stop -> exit(server_stoped)
		end.
%-------------------------------------------------------------------------------
create_game(Dict, From, Name, NumPlayers) ->
    	case dict:is_key(Name, Dict) of
        	true ->
            		Message = lists:flatten(io_lib:format(
					"Name '~w' already in use!", [Name])),
					From ! {collector, {error, Message}},
					main_loop(Dict);
               
			false -> Deck = shuffleDeck(),
            		{NewDeck, Hands} = giveCards(NumPlayers, Deck),
					Turns = lists:seq(1, NumPlayers),
					NewPid = spawn(fun() -> game_loop(1, NumPlayers, NewDeck, Hands, [], Turns, 0) end),
					link(NewPid),
					From ! {collector, {NewPid, 1}},
					main_loop(dict:store(Name, {NewPid, NumPlayers, 1}, Dict))
		end.
%-------------------------------------------------------------------------------
do_drop(Turn, Card, Hands, Dropped, Turns, Deck, NumPlayers, Finished) ->
        Hand = dict:fetch(Turn, Hands),
        Cards = lists:delete(Card, Hand),
        NewHands = dict:store(Turn, Cards, Hands),
        NewDropped = [Card | Dropped],
        NewTurn = next_turn(Turn, NumPlayers),
        game_loop(NewTurn, NumPlayers, Deck, NewHands, NewDropped, Turns, Finished).
       
%-------------------------------------------------------------------------------
game_loop(Turn, NumPlayers, Deck, Hands, Dropped, Turns, Finished) ->

	receive
    	{turn, From} ->
        		From ! {self(), Turn},
				game_loop(Turn, NumPlayers, Deck, Hands, Dropped, Turns, Finished);

        {the_end, From} ->
            	From ! {self(), checkEnd(Hands, Deck)},
				game_loop(Turn, NumPlayers, Deck, Hands, Dropped, Turns, Finished);
       
        {sneak, From, Turn} ->
            	From ! {self(), sendCards(Turn, Hands, Dropped)},
				game_loop(Turn, NumPlayers, Deck, Hands, Dropped, Turns, Finished);
       
        {drop, From, Turn, Card} ->
            	From ! ok,
				do_drop(Turn, Card, Hands, Dropped, Turns, Deck, NumPlayers, Finished);
       
        {pick, deck, From, Turn} ->
            	From ! ok,
				do_pick_from_deck(Turn, NumPlayers, Deck, Hands, Dropped, Turns, Finished);
       
        {pick, dropped, From, Turn, Card} ->
            	From ! ok,
				do_pick_from_dropped(Turn, NumPlayers, Deck, Hands, Dropped, Turns, Card, Finished);

        {finished_picking, From} ->
            	From ! ok,
				NewFinished = Finished + 1,
				case NewFinished == NumPlayers of
                		true -> NewDeck = Deck ++ Dropped,
								NewTurn = next_turn(Turn, NumPlayers),
								game_loop(NewTurn, NumPlayers, NewDeck, Hands, [], Turns, 0);
						false -> 
								game_loop(Turn, NumPlayers, Deck, Hands, Dropped, Turns, NewFinished)
				end
       
		after ?TIME_OUT -> ok % End game loop after time out.
   
    end.
%-------------------------------------------------------------------------------
do_pick_from_deck(Turn, NumPlayers, [], Hands, Dropped, Turns, Finished) ->
		NewTurn = next_turn(Turn, NumPlayers),   
		game_loop(NewTurn, NumPlayers, [], Hands, Dropped, Turns, Finished);

do_pick_from_deck(Turn, NumPlayers, [C1|[]], Hands, Dropped, Turns, Finished) ->
		Hand = dict:fetch(Turn, Hands),
		NewHand = Hand ++ [C1],
		NewHands = dict:store(Turn, NewHand, Hands),
		NewTurn = next_turn(Turn, NumPlayers),   
		game_loop(NewTurn, NumPlayers, [], NewHands, Dropped, Turns, Finished);
		
do_pick_from_deck(Turn, NumPlayers, [C1, C2 | T], Hands, Dropped, Turns, Finished) ->
		Hand = dict:fetch(Turn, Hands),
		NewHand = Hand ++ [C1, C2],
		NewDeck = T,
		NewHands = dict:store(Turn, NewHand, Hands),
		NewTurn = next_turn(Turn, NumPlayers),   
		game_loop(NewTurn, NumPlayers, NewDeck, NewHands, Dropped, Turns, Finished).   
%-------------------------------------------------------------------------------
do_pick_from_dropped(Turn, NumPlayers, Deck, Hands, Dropped, Turns, Card, Finished) ->
    	Hand = dict:fetch(Turn, Hands),
		NewHand = Hand ++ [Card],
		NewHands = dict:store(Turn, NewHand, Hands),
		NewDropped = lists:delete(Card, Dropped),
		NewTurn = next_turn(Turn, NumPlayers),
		game_loop(NewTurn, NumPlayers, Deck, NewHands, NewDropped, Turns, Finished).
%-------------------------------------------------------------------------------
join_game(Dict, From, Name) ->
        case dict:find(Name, Dict) of
        	{ok, {Pid, NumPlayers, ActualPlayers}} ->
            	case ActualPlayers < (NumPlayers - 1) of
                	true ->
                    	From ! {collector, {Pid, ActualPlayers + 1}},
                        main_loop(dict:store(Name, {Pid, NumPlayers, ActualPlayers + 1}, Dict));
					false ->
                            From ! {collector, {Pid, ActualPlayers + 1}},
                            main_loop(dict:erase(Name, dict:store(Name, {Pid, NumPlayers, ActualPlayers + 1}, Dict)))
                end;
            error ->
            	Message = lists:flatten(io_lib:format("Game '~w' not available!", [Name])),
                From ! {collector, {error, Message}},
                main_loop(Dict)
        end.
%-------------------------------------------------------------------------------
available_games(Dict, From) -> 
		From ! {collector, dict:fetch_keys(Dict)},
        main_loop(Dict).
%-------------------------------------------------------------------------------
remove_dead_process(DeadPid, Dict) ->
     	dict:filter(fun (_, Pid) -> Pid /= DeadPid end, Dict).
%-------------------------------------------------------------------------------
shuffleDeck() -> shuffleDeck(?DECK, []).
%-------------------------------------------------------------------------------
shuffleDeck([], N) -> N;
shuffleDeck(L, N) ->
     	RandNum = random:uniform(length(L)),
		CardInsert = lists:nth(RandNum, L),
		DecList = lists:delete(CardInsert, L),
		shuffleDeck(DecList, [CardInsert|N]).
%-------------------------------------------------------------------------------
fullHouse(Hand) ->
    	if
        	length(Hand) >= 13 ->
            	case lists:all(fun(X) -> lists:member(X, Hand) end, ?DECK) of
                	true -> true;
					false -> false
				end;
			length(Hand) < 13 -> false
		end.
%-------------------------------------------------------------------------------
checkEnd(Hands, Deck) ->
		case Deck == [] of
			false ->
    			Dict = dict:filter(fun(_, V)-> fullHouse(V) end, Hands),
				Win = dict:fetch_keys(Dict),
				case Win of
        			[] -> false;
					_-> hd(Win)
				end;
			_ -> false
    	end.
%-------------------------------------------------------------------------------
next_turn_list([H|T]) -> T ++ [H].

next_turn(Turn, NumPlayers) ->
    	case Turn < NumPlayers of
         	true -> Turn + 1;
         	false -> 1
		 end.
%-------------------------------------------------------------------------------
giveCards(NumPlayers, Deck) -> giveCards(NumPlayers, Deck, dict:new()).

giveCards(0, Deck, Dict) -> {Deck, Dict};
giveCards(N, [C1, C2, C3, C4, C5 | T], Dict) ->
    	Hand = [C1, C2, C3, C4, C5],
    	NewDict = dict:store(N, Hand, Dict),
    	giveCards(N - 1, T, NewDict).
%-------------------------------------------------------------------------------
sendCards(Turn, Hands, Dropped) ->
        Hand = dict:fetch(Turn, Hands),
        OtherHands = sneak(dict:filter(fun (K, _) -> K /= Turn end, Hands)),
        {Hand, OtherHands, Dropped}.
%-------------------------------------------------------------------------------
sneak(Hands) ->
    	HandList = dict:to_list(Hands),
		HowMany = lists:map(fun({Who, Hand}) -> {Who, length(Hand)} end, HandList),
		HowMany.
