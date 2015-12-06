:- module(webPlayer, []).


initialize(PlayerName, PlayerCount) :-
	show(1, 'I am ~w of a ~w player game.~n', [PlayerName, PlayerCount]).

getGems(_,_).
buyCard(_).
reserveCard(_).
reserveCardFromDeck(_).
selectNoble(_,_) :- false.

decideAction(Player, Oponents, StateProxy, Action) :- Action=none
	.
