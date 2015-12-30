% Ilhan Adiyaman
% 2015700000
% Maximum points oriented agent.
% This agent try to purchase with the highest point in the available card list.
% If the points are equal then picks the one which helps to receive a noble.
%

:- module(simple2015700000, []).

initialize(PlayerName, PlayerCount) :-
	show(1, 'I am ~w of a ~w player game.~n', [PlayerName, PlayerCount]).

getGems(_,_).
buyCard(_).
reserveCard(_,_).
reserveCardFromDeck(_).

calculateNoblesImportance(Nobles,NewImportance):-
	retractall(importanceColor(_)),
	assert(importanceColor([0,0,0,0,0,0])),
	forall( (member(_-X2-_, Nobles)),
		(
			importanceColor(Importance),addGems(Importance, X2, New1),retractall(importanceColor(_)), assert(importanceColor(New1))
			;
			true
		)
	),
	importanceColor(Result), NewImportance = Result,
	nl,write('IMPORTANCE:'),write(NewImportance),nl.

calculateCardsImportance(Cards,NewImportance):-
	retractall(importanceColor(_)),
	assert(importanceColor([0,0,0,0,0,0])),
	forall( (member(CardId, Cards)),
		(
			card(CardId, RequiredGems-_-_),importanceColor(Importance),addGems(Importance, RequiredGems, New1),retractall(importanceColor(_)), assert(importanceColor(New1))
			;
			true
		)
	),
	importanceColor(Result), NewImportance = Result,
	write('IMPORTANCECARDS:'),write(NewImportance),nl.

decideCardToBuy(CanBuyCards,ImportantCards):-
	b_sort(CanBuyCards,[],ImportantCards).

b_sort([],Acc,Acc).
b_sort([H|T],Acc,ImportantCards):-
	bubble(H,T,NT,Max),b_sort(NT,[Max|Acc],ImportantCards).
   
bubble(X,[],[],X).
bubble(X,[Y|T],[Y|NT],Max):-
	card(X, _-_-PointsX),card(Y, _-_-PointsY),PointsX>PointsY,bubble(X,T,NT,Max).
bubble(X,[Y|T],[X|NT],Max):-
	card(X, _-_-PointsX),card(Y, _-_-PointsY),PointsX<PointsY,bubble(Y,T,NT,Max).
bubble(X,[Y|T],[X|NT],Max):-
	card(X, _-_-PointsX),card(Y, _-_-PointsY),PointsX==PointsY,compareColors(X,Y,Result),bubble(Result,T,NT,Max).

compareColors(X,Y,Result):-
	card(X, _-ColorX-_),card(Y, _-ColorY-_),importanceColor(Importance),importanceData(ColorX,Importance,XVal),importanceData(ColorY,Importance,YVal),
	(
		XVal > YVal, Result = X;
		XVal =< YVal, Result = Y
	).

importanceData(A, [N1,N2,N3,N4,N5,0], L):- 
	A=1, L=N1;
	A=2, L=N2;
	A=3, L=N3;
	A=4, L=N4;
	A=5, L=N5.


% Remove X from the list
del(X, [X | Tail], Tail).
del(X, [Y | Tail], [Y | Tail1]):- del(X, Tail, Tail1).

last(X,[X]).
last(X,[_|Z]):- last(X,Z).

set([],[]).
set([H|T],[H|T1]) :- subtract(T,[H],T2), set(T2,T1).

decideAction(Player, Oponents, StateProxy, Action) :-
	call(StateProxy, Player, gems, Gems),
	call(StateProxy, Player, bonuses, Bonuses),
	call(StateProxy, Player, reserves, Reserves),
	call(StateProxy, game, cards, Cards),
	call(StateProxy, game, nobles, Nobles),
	calculateNoblesImportance(Nobles,_),
	(
		canBuyCards(Gems, Bonuses, Cards, CanBuyCards),
		decideCardToBuy(CanBuyCards, ImportantCards),
		write('IMPORTANT:'),write(ImportantCards),nl,
		last(CardId,ImportantCards),
		Action = buyCard(CardId)
		;
		call(StateProxy, game, tokens, Tokens),
		decideCardToBuy(Cards, SortedCards),
		write('CARDSONBOARD:'),write(Cards),nl,
		write('CARDS:'),write(SortedCards),nl,
		set(SortedCards, Removed),
		write('Removed:'),write(Removed),nl,
		decideGemToGets(Removed, Bonuses,Gems, Tokens, RandGems, BackGems),
		%randomGetGems(Gems, Tokens, RandGems, BackGems),
		Action = getGems(RandGems, BackGems)
	)
	,member(Oponent, Oponents)
	,call(StateProxy, Oponent, score, _)
	.

decideGemToGets(SortedCards, Bonuses, Gems, Tokens, GetGems, BackGems):-
	retractall(sortedCardList(_)),
	assert(sortedCardList(SortedCards)),
	addGems(Bonuses, Gems, Total),
	forall( (member(CardId, SortedCards)),
		(
			sortedCardList(AvailableListCards),
			card(CardId, RequiredGems-BonusColor-Points), removeGems(RequiredGems, Total, Result),
			gemCount(Result, N), N > 3, del(CardId, AvailableListCards, NewList),retractall(sortedCardList(_)), assert(sortedCardList(NewList)),
			write('CardId:'),write(CardId),nl, 
			write('RequiredGems:'),write(RequiredGems),nl, 
			write('Gems:'),write(Total),nl, 
			write('Result:'),write(Result),nl
			;
			true
		)
	),
	sortedCardList(AvailableCards),
	write('AvailableCards:'),write(AvailableCards),nl,
	calculateCardsImportance(AvailableCards, Importance),
	write('ImportanceCards:'),write(Importance),nl,
	subtractGems([1,1,1,1,1,0],Importance,K),
	removeToGems(K,[0,0,0,0,0,0],L), makeZeros(L,M),
	randomGetGems([0,0,0,0,0,0],M,GetGems,BackGems),
	write('GetGems:'),write(GetGems),nl.


selectNoble([H|_],H).

canBuyCards(_, _, [], []).

canBuyCards(Gems, Bonuses, [H|T], A) :-
	(
		canBuyCard(Gems, Bonuses, H),
		A = [H|X2]
		;
		A=X2
	),
	canBuyCards(Gems, Bonuses, T, X2).


removeToGems([A1,A2,A3,A4,A5,A6], [B1,B2,B3,B4,B5,B6], [C1,C2,C3,C4,C5,C6]) :- 
	C1 is max(A1-B1,1),
	C2 is max(A2-B2,1),
	C3 is max(A3-B3,1),
	C4 is max(A4-B4,1),
	C5 is max(A5-B5,1),
	C6 is max(A6-B6,0).

makeZeros([A1,A2,A3,A4,A5,A6], [C1,C2,C3,C4,C5,C6]) :-
	(	
		A1>1, C1 is 0
		; 
		A1=<1, C1 is A1
	),
	(	
		A2>1, C2 is 0
		; 
		A2=<1, C2 is A2
	),
	(	
		A3>1, C3 is 0
		; 
		A3=<1, C3 is A3
	),
	(	
		A4>1, C4 is 0
		; 
		A4=<1, C4 is A4
	),
	(	
		A5>1, C5 is 0
		; 
		A5=<1, C5 is A5
	),
	C6 is 0.