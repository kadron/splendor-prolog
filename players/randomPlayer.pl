:- module(randomPlayer, []).
/*
:- dynamic cards/1.
:- dynamic tickets/1.

cards([]).
tickets([]).
*/

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
	call(StateProxy, game, tokens, Tokens),
	(
		length(Reserves, ReservesLength),
		ReservesLength<3,
		random(1,11,1),
		length(Cards, CardsLength),
		CardsLength1 is CardsLength+1,
		random(1, CardsLength1, ReserveId),
		nth1(ReserveId, Cards, ReservedCard),
		getReserveBackGems(Gems, Tokens, ReserveBackGems),
		Action = reserveCard(ReservedCard, ReserveBackGems)
		;
		canBuyCards(Gems, Bonuses, Cards, CanBuyCards),
		nth1(1, CanBuyCards, CardId),
		Action = buyCard(CardId)
		;
		randomGetGems(Gems, Tokens, RandGems, BackGems),
		Action = getGems(RandGems, BackGems)
	)
	,member(Oponent, Oponents)
	,call(StateProxy, Oponent, score, _)
	.

selectNoble([H|_],H).

canBuyCards(_, _, [], []).

canBuyCards(Gems, Bonuses, [H|T], A) :-
	(
		canBuyCard(Gems, Bonuses, H),
		A = [H|X2]
		;
		A=X2
	),
	canBuyCards(Gems, Bonuses, T, X2)
	.

getReserveBackGems(Gems, Tokens, BackGems) :-
	nth1(6, Tokens, GoldTokens),
	GoldenTokenCount is min(1, GoldTokens),
	addGems(Gems, [0,0,0,0,0,GoldenTokenCount], NewGems),
	gemCount(NewGems, GemCount),
	ExcessGemCount is GemCount -10,
	(ExcessGemCount=<0,BackGems=[0,0,0,0,0,0];ExcessGemCount>0,randomGems(NewGems, ExcessGemCount, false, BackGems)).

randomTest(A) :-
	A=3,
	display('hi there'),
	repeat,
	random(1, 10000000, X),
	X<5,
	!.