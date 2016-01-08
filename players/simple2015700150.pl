:- module(simple2015700150, []).

initialize(PlayerName, PlayerCount) :-
	show(1, 'I am ~w of a ~w player game.~n', [PlayerName, PlayerCount]).

getGems(_,_).
buyCard(_).
reserveCard(_).
reserveCardFromDeck(_).

decideAction(Player, Oponents, StateProxy, Action) :-
	call(StateProxy, Player, gems, Gems),
	call(StateProxy, Player, bonuses, Bonuses),
	call(StateProxy, Player, reserves, Reserves),
	call(StateProxy, game, cards, Cards),
	(	
		%first tries to buy a card because the most important thing for a player to gain point
		canBuyCards(Gems, Bonuses, Cards, CanBuyCards),
		length(CanBuyCards,L),
		L > 0,
		findMaxPointCard(CanBuyCards, CardId),
		Action = buyCard(CardId)
		;
		%if it cannot take a card, takes tokens 
		call(StateProxy, game, tokens, Tokens),
		randomGetGems(Gems, Tokens, RandGems, BackGems),
		Action = getGems(RandGems, BackGems)
		;
		length(Reserves, ReservesLength),
		ReservesLength<3,
		length(Cards, CardsLength),
		CardsLength1 is CardsLength+1,
		random(1, CardsLength1, ReserveId),
		nth1(ReserveId, Cards, ReservedCard),
		Action = reserveCard(ReservedCard)
	).

selectNoble([H|_],H).

canBuyCards(_, _, [], []).

canBuyCards(Gems, Bonuses, [H|T], A) :-
	(
		canBuyCard(Gems, Bonuses, H),
		A = [H|X2]
		;
		A = X2
	),
	canBuyCards(Gems, Bonuses, T, X2)
	.

:- dynamic pointsOfBuyableCards/1.

findMaxPointCard(Cards, Card) :-
	retractall(pointsOfBuyableCards(_)),
	assert(pointsOfBuyableCards([])),
	length(Cards, L),
	reverse(Cards, RCards),
	forall(between(1, L, Index),
		(
			nth1(Index, RCards, ACard),
			cardDataRaw(_,ACard,_,_,_,_,_,Point,_,_,_,_,_),
			pointsOfBuyableCards(Points),
			retractall(pointsOfBuyableCards(_)),
			assert(pointsOfBuyableCards([Point|Points]))
		)
	),
	pointsOfBuyableCards(Points),
	max_list(Points, MaxPoint),
	nth1(Ind, Points, MaxPoint),
	nth1(Ind, Cards, Card).
