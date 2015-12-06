:- module(human, []).


initialize(PlayerName, PlayerCount) :-
	show(1, 'I am ~w of a ~w player game.~n', [PlayerName, PlayerCount]).

getGems(_,_).
buyCard(_).
reserveCard(_).
reserveCardFromDeck(_).

selectNoble(L,NobleId-X-C) :- 
	display('available nobles: '),
	display(L),
	display('~nselect noble: '),
	read(NobleId),
	member(NobleId-X-C, L),!
	.

showState(Title, StateProxy, X) :-
	format('~n~w~n', [Title]),
	setof(B-C, (call(StateProxy, X, B, C)),Z),
	findall(_, (member(E-F, Z), format('~w: ~w~n', [E, F])), _).

decideAction(Player, Oponents, StateProxy, Action) :-
	showState('GAME STATE', StateProxy, game),
	findall(_, (member(Oponent, Oponents), showState('OPONENT STATE', StateProxy, Oponent)),_),
	showState('YOUR STATE', StateProxy, Player),
	display('your action: '),
	read(Action)
	.
