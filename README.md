# splendor-prolog
Ersin Basaran's Implementation of Splendor.

# Updates
###### 31/12/2015: doTournament/1 and doTournament/2 predicates are added
The frame now includes a new predicate to perform complete tournaments. In order to run the tournament use one of the followings:

    doTournament(50).
    doTournament([Player1, Player2, ..., PlayerN], 50).

The first one, gets all the players in players folder, runs the tournament and prints the result. If you want to have a tournament with a subset of players, use second one by providing the list of players. In this example, each player plays 100 games with each other players. 

###### 30/12/2015: runGameBatch predicate is added

The frame now includes a new predicate 

    runGameBatch([P1, P2], Count, P1WinCount, P2WinCount)

Example usage:

    runGameBatch([randomPlayer, randomPlayer], 50, P1WinCount, P2WinCount).

which returns player win counts as P1WinCount and P2WinCount after simulating 100 games (50 with P1 vs P2, 50 with P2 vs P1).

If any draw happens (unlikely but possible), it counts both players as winners. Therefore 

    P1WinCount + P2WinCount = 100 + drawCount

# Getting Started 

In order to run the game the splendor.pl file must be consulted first.

	[splendor].

To start a game, runGame predicate can be used. It initializes the board and players and run the game in batch mode without requiring any user interaction unless a human player is selected. 

# Players

The players are located in players/ subfolder. There are currently 3 players:

- human.pl: when selected, the user is asked to enter the action using command line
- randomPlayer.pl: performs a random move in each step. With 10% chance it reserves one of the open cards (if less then 3 cards reserved previously). If it does not reserve, it checks if it can get a card with its current gems. If there are no such cards it selects 3 (or 2) gems randomly. If there are excess gems, it also selects the gems to return randomly. 
- webPlayer.pl: This player is used by 'webplay.pl' file and it basically does nothing. It always returns 'none' as the action. This allows 'webplay.pl' file to wait for the used input. 

# How to Run the Game

## In Console Mode
 
To run the game in console or batch mode the following command should be run:

	runGame([<LIST_OF_PLAYERS>]).

Ex:
	runGame([human, randomPlayer, randomPlayer]).

starts a new game with 3 players: a command line human player, and two randomPlayers.

## Using a web browser

In order to start game in the browser (webplay mode), first consult 'webplay.pl' file and run startServer command.

	[webplay].
	startServer(5010).

This will start a webserver on localhost at port 5010 and will launch the game page in the browser. Within the browser, the user can start a new game with selected number of players. Current UI does not allow to select the player types or order. However it is doable. The human player is always at the first position and all other players are 'randomPlayer'.

# Player and Framework Interaction

Players are not allowed to send an action to the framework. The framework, itself, asks an agent to select an action, when it is its turn. Therefore, each player should have the following predicate defined:
	
	decideAction(Player, Oponents, StateProxy, Action) :- ...

The Action needs to be one of the following:

	getGems(GetGems,SendBackGems).
	buyCard(CardId).
	reserveCard(CardId, SendBackGems).
	reserveCardFromDeck(DeckId, SendBackGems).

	GetGems and SendBackGems are lists with 6 elements like [0,1,1,1,0,0], first five numbers denoting regular tokens and last number denoting gold token.
	CardId is the card's ID number (e.g. 23 or 215).
	DeckID is a number between 1 and 3.

These predicates are used only for telling the action and matching its parameters. They are not evaluated. 

Also the framework may invoke selectNoble predicate of the player to let it select from one of the noble cards in the NobleList. 

	selectNoble(+NobleList,-SelectedNoble).

Player corresponds to the player itself :) Used for getting its own state (gems, bonusses etc). Oponents is a list of oponents. Can be used to fetch their states. 

The framework supplies the information about the game state and player states using StateProxy predicate:

	get owned gems:
		call(StateProxy, Player, gems, Gems),

	get owned bonus points:
		call(StateProxy, Player, bonuses, Bonuses),

	get reserved cards:
		call(StateProxy, Player, reserves, Reserves),

	get acquired nobles:
		call(StateProxy, Player, nobles, Nobles),

	get score:
		call(StateProxy, Player, score, Score),

	get all opened cards
		call(StateProxy, game, cards, Cards),

	get close cards (**new**) (`Cards` is a list `[D1,D2,D3]` which includes the close cards in each deck and they are shuffled at each call)
		call(StateProxy, game, closeCards, Cards),

	get available nobles 
		call(StateProxy, game, nobles, Nobles),

	get one openent's score
		call(StateProxy, Oponent, score, OponentScore)


StateProxy allows framework to hide the corresponding dynamic predicate from the players to prevent hacks. 

# Logging

The framework has 'show' predicate, which can be used for logging.

	Ex.
		show(10, 'Open cards: ~w~n', [L1])

displays the text 'Open cards: ' followed by a list defined by L1 in the console. The first parameter is importance parameter. 0 means most important, 100 means least important (or any positive number of your choice). 

The verbosity can be set by setVerbose(+N) predicate. When 'setVerbose(10)' is called all messages that have importance less then or equal to N are displayed, the others are ignored. setVerbose(-1) causes no output to be presented. 

All outputs are also passed to the browser console. This enables to get some debugging messages when an action fails (like invalid gem action).