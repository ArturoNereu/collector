%% ITESM CEM, May 10, 2010.
%% Erlang Source File
%% Activity: Collector
%% Author: Artruro Nereu Nunez Martinez, 1163145
%% Author: Diego Arturo Gurrusquieta Quezada, 1163085
%% Author: Juan Manuel Fernandez Perez 1162937

-module(client).
-compile(export_all).

-define(SLEEP_DELAY, 1000).

start(Server) ->
	titles(),
	case testServer(Server) of 
		ok -> main_menu(Server);
		fail -> io:format("Error al iniciar el servidor~n")
	end.
	
main_menu(Server) ->
    io:format("~n===========~n"),
    io:format(" MAIN MENU~n"),    
    io:format("===========~n~n"),
    io:format("  (1) Create a new game~n"),
    io:format("  (2) Join an existing game~n"),
    io:format("  (3) Exit~n~n"),
    
    case read_input(1, 3) of
    
        1 ->	create(Server),
                main_menu(Server);
                
        2 ->    join(Server),
                main_menu(Server);
                
        3 ->    ok
    end.    

	
testServer(Server) ->
	case rpc:call (Server, server, test, []) of
		ok -> ok;
		{badrpc, _Reason} -> error_message(lists:flatten(io_lib:format("Can't connect to ~w!", [Server]))),
										  fail
	end.

titles() ->
	io:format("~nDistributed Erlang Collector Game, Version 1.0"),
    io:format("~n(C) by Arturo Nereu|Diego Gurrusquieta|Manuel Fernandez, 2010.~n").
	
read_input(Min, Max) ->
    Prompt = list_to_atom(lists:flatten(
        io_lib:format("~nPlease select a number between ~w and ~w: ", 
                      [Min, Max]))),    
    case io:fread(Prompt, "~d") of
        {ok, [Result]} ->
            if
                (Result < Min) or (Result > Max) -> 
                    read_input(Min, Max);  
                true -> Result
            end;
        {error, _} -> read_input(Min, Max)     
    end.

create(Server) ->
    io:nl(),
    case io:fread('Specify the name of the game: ', "~a") of
        {ok, [Name]} ->
			NumPlayers = read_input(2, 4),
            case rpc:call(Server, server, create, [Name, NumPlayers]) of
            
                {badrpc, _Reason} ->
                    error_message(lists:flatten(io_lib:format(
                        "Can't connect to ~w!", [Server])));
                    
                {error, Message} ->
                    error_message(Message);
                    
                {Pid, Symbol} ->
                    link(Pid),
                    game(Pid, Symbol, see)                
            end        
    end.
	
error_message(Message) ->
    io:format("~n*** ERROR ***~n"),
    io:format(Message),
    io:nl().    	
	
join(Server) ->
 
    case rpc:call(Server, server, available, []) of
    
        {badrpc, _Reason} ->
            	error_message(lists:flatten(io_lib:format(
                "Can't connect to ~w!", [Server])));
                
		 Games ->        
            case Games of
                [] -> 
                    io:format("~nSorry, no games available.~n");
                    
                _ ->
                    available(Games),
                    case io:fread(
                        'Specify the name of the game to join: ', 
                        "~a") of
                        
                        {ok, [Name]} ->
                            case rpc:call(Server, server, join, [Name]) of
                                {error, Message} ->
                                    error_message(Message);
                                {Pid, Symbol} ->
                                    link(Pid),
                                    game(Pid, Symbol, see)
                            end            
                    end        
            end

    end.     
	
game(Pid, Symbol, State) ->
	
    case is_game_over(Pid, Symbol) of
    
        true -> ok;
            
        false ->
            io:format("~nJust a moment "),
			wait_for_turn(Pid, Symbol),
			
			case State of
			
				see -> 	
						showCards(Pid, Symbol), 
						game(Pid, Symbol, drop);
				
				drop ->
						pickToDrop(Pid, Symbol),
						game(Pid, Symbol, pickSource);
					
				pickSource ->  
						io:format("~n---------------------------------------------"),
						io:format("~nChoose where to take your cards!!~n"),
						showCards(Pid, Symbol),
						fromWhere(Pid, Symbol),
						call(Pid, {finished_picking, self()}),
						game(Pid, Symbol, see)
			
			end
    end.
	
fromWhere(Pid, Symbol) ->
	case read_selection(1,2) of
	
		1 -> call(Pid, {pick, deck, self(), Symbol});
			
		_ -> pickToGrabFromDropped(Pid, Symbol)
	
	end.
	
pickToGrabFromDropped(Pid, Symbol) ->
	{Pid, {_, Others, Dropped}} = call(Pid, {sneak, self(), Symbol}),
	printCardsPos(Dropped, Others),
	case read_input(1, length(Dropped)) of
	
		X -> call(Pid, {pick, dropped, self(), Symbol, lists:nth(X, Dropped)})
		
	end.	
	
printCardsPos(Dropped, Others) ->    
	io:format("~n~nDropped cards:"),
    lists:foreach(fun (X) -> io:format(" ~p ", [X]) end, Dropped),
	io:nl(),
	io:format("              "),
	lists:foreach(fun (X) -> io:format(" ~p ", [X]) end, lists:seq(1, length(Dropped))),
	lists:foreach(fun ({X, Y}) -> io:format("~nJugador: ~p, Cartas: ~p ", [X, Y]) end, Others).	
	
read_selection(Min, Max) ->
    Prompt = list_to_atom(lists:flatten(
        io_lib:format("~nSelect your source ~n1)Deck~n2)Dropped~n",[]))),    
    case io:fread(Prompt, "~d") of
        {ok, [Result]} ->
            if
                (Result < Min) or (Result > Max) -> 
                    read_input(Min, Max);  
                true -> Result
            end;
        {error, _} -> read_input(Min, Max)     
    end.
	
pickToDrop(Pid, Symbol) ->
	io:format("~n---------------------------------------------"),
	io:format("~nSelect a card from your hand and drop it!!~n"),
	{Pid, {MyCards, Others, Dropped}} = call(Pid, {sneak, self(), Symbol}),
	printCardsPos(MyCards, Others, Dropped),
	case read_input(1, length(MyCards)) of
	
		X -> call(Pid, {drop, self(), Symbol, lists:nth(X, MyCards)})
		
	end.	 
	
printCardsPos(Hand, Others, Dropped) ->    
		io:format("~n~nDropped cards:"),
		lists:foreach(fun (_) -> io:format(" * ") end, Dropped),
		lists:foreach(fun ({X, Y}) -> io:format("~nPlayer: ~p, Cards: ~p ", [X, Y]) end, Others),
		io:format("~n~nYour cards:"),
		lists:foreach(fun (X) -> io:format(" ~p ", [X]) end, Hand),
		io:format("~n           "),
		lists:foreach(fun (X) -> io:format(" ~p ", [X]) end, lists:seq(1, length(Hand))).

showCards(Pid, Symbol) ->
	{Pid, {MyCards, _, _}} = call(Pid, {sneak, self(), Symbol}),
	printCards(MyCards).
	
printCards(Hand) -> 
    io:format("~n~nYour cards:"),
    lists:foreach(fun (X) -> io:format(" ~p ", [X]) end, Hand),
    io:nl().
	
available(Games) ->    
    io:format("~nAvailable games:~n"),
    lists:foreach(fun (X) -> io:format("    ~s~n", [X]) end, Games),
    io:nl().
	
call(P, M) ->
    P ! M,
    receive
        X -> X
    end.   	

is_game_over(Pid, Symbol) ->
    case call(Pid, {the_end, self()}) of
        {Pid, false}        -> false;
        {Pid, X}            -> game_over(Symbol, X), true
    end.
	
game_over(Symbol, X) ->
    io:format("~nGAME OVER ~n"),
    case X of
        tie     -> io:format("It was a tie!");
        Symbol  -> io:format("The Force is strong with this one!!! You won!");
        _       -> io:format("I find your lack of faith disturbing!!! You lost!")
    end,
    io:nl().		
	
       
wait_for_turn(Pid, Symbol) ->    
    case call(Pid, {turn, self()}) of        
        {Pid, Symbol} -> ok;         
        {Pid, _} ->
            io:format("."),
            timer:sleep(?SLEEP_DELAY),
            wait_for_turn(Pid, Symbol) 
    end.
