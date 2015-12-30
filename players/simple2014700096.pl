:- module(simple2014700096, []).

initialize(PlayerName, PlayerCount) :-
	show(1, 'I am ~w of a ~w player game.~n', [PlayerName, PlayerCount]).

getGems(_,_).
buyCard(_).
reserveCard(_).
reserveCardFromDeck(_).

bonusGem(1, [1,0,0,0,0,0]).
bonusGem(2, [0,1,0,0,0,0]).
bonusGem(3, [0,0,1,0,0,0]).
bonusGem(4, [0,0,0,1,0,0]).
bonusGem(5, [0,0,0,0,1,0]).


decideAction(Player, _, StateProxy, Action) :-
	call(StateProxy, Player, gems, Gems),
	call(StateProxy, Player, bonuses, Bonuses),
	call(StateProxy, Player, reserves, Reserves),
	call(StateProxy, game, cards, Cards),
	call(StateProxy, game, nobles, Nobles),
	(
		canBuyCards(Gems, Bonuses, Cards, CanBuyCards),
		cardPointList(StateProxy, Player, CanBuyCards, PointList, Nobles),
		maximum_list(PointList, _, Index),
		nth1(Index, CanBuyCards, CardId),
		%nl,write(CanBuyCards),nl,write(PointList),nl,
		Action = buyCard(CardId)
		;
		length(Reserves, ReservesLength),
		ReservesLength<3,
		nth1(2, Cards, ReservedCard),
		Action = reserveCard(ReservedCard)
		;
		call(StateProxy, game, tokens, Tokens),
		randomGetGems(Gems, Tokens, RandGems, BackGems),
		Action = getGems(RandGems, BackGems)
	)
	.


cardPointList(_,_,[],[],_):-!.
cardPointList(StateProxy,Player,[CardId|CT], [PH|PT], Nobles):-
	%initialCards(L1,L2,L3),
	%append([L1,L2,L3], AllCards),
	%member(CardId, AllCards),
	
	card(CardId, _-BonusColor-Points),
	
	call(StateProxy, Player, bonuses, Bonuses),
	bonusGem(BonusColor, BonusGem),
	addGems(Bonuses, BonusGem, NewBonuses),
	canGetNobles(NewBonuses, NobleGets, Nobles),
	
	length(NobleGets, NobleGetCount),
	(
		NobleGetCount = 0,
		PH is Points
		;
		PH is Points+3
	),

	cardPointList(StateProxy,Player,CT,PT,Nobles),!.
	
	
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
	
canGetNobles(Bonusses, NobleGets, Nobles) :-
	findall(
		NId-X-C, 
		(
			member(NId-X-C,Nobles),
			subtractGems(Bonusses, X, BonusGemRemain),
			splendor:minGem(BonusGemRemain, MinC),
			MinC >= 0
		),
		NobleGets
	).

	
% LIST OPS
insert_at(E,L,1,[E|L]):-!.
insert_at(E,[H|T],Num,[H|L2]):-
	Num > 1,
	NewNum is Num-1,
	insert_at(E,T,NewNum,L2).
	
remove_at(H,[H|T],1,T):-!.
remove_at(E,[H|T],Num,[H|L2]):-
	Num > 1,
	NewNum is Num - 1,
	remove_at(E,T,NewNum,L2).
	
maximum_list(L, M, I) :- nth1(I, L, M), \+ (member(E, L), E > M),!.
