:- module(s2014700171, []).

:- dynamic bonuses/1.
:- dynamic reserves/1.
:- dynamic gems/1.
:- dynamic nobleTiles/1.
:- dynamic targets/1.

bonuses([0,0,0,0,0,0]).
reserves([]).
gems([0,0,0,0,0,0]).
nobleTiles([]).
targets([]).

initialize(PlayerName, PlayerCount) :-
	show(1, 'I am ~w of a ~w player game.~n', [PlayerName, PlayerCount]).
	

getGems(_,_).
buyCard(_).
reserveCard(_).
reserveCardFromDeck(_).

stateProxy(game, nobles, L) :-
findall(X,splendor:nobles(X),L).


initializeTarget(StateProxy):-
	stateProxy(game, nobles, Nobles),
	length(Nobles,L),
	L1 is L+1,
	random(1,L1,Number),
	nth1(Number,Nobles,NobleCard),
	retract(targets(_)),
	assert(targets([NobleCard])).




decideAction(Player, Oponents, StateProxy, Action) :-
	targets(Targets),
	length(Targets,L),
	L=0,
	initializeTarget(StateProxy),
	decideAction(Player, Oponents, StateProxy, Action).


decideAction(Player, Oponents, StateProxy, Action) :-
	splendor:stateProxy( Player, gems, Gems),
	splendor:stateProxy( Player, bonuses, Bonuses),
	splendor:stateProxy( Player, reserves, Reserves),
	splendor:stateProxy( game, cards, Cards),
	splendor:stateProxy( game, tokens, Tokens),
	(
	targets([NobleCard]),
	needBonuses(NobleCard,Bonuses,NeedBonuses),
	canBuyCards(Gems, Bonuses, Cards, CanBuyCards),
	selectCard(NeedBonuses,CanBuyCards,Card),
	Action = buyCard(Card);
	
	canBuyCards(Gems, Bonuses, Cards, CanBuyCards),
	nth1(1, CanBuyCards, CardId),
	Action = buyCard(CardId);
		
	closestCardToBuy(Gems,Bonuses,Cards,RequiredGems),
	selectGems(Gems,RequiredGems,SelectedGems,BackGems,Tokens),
	gemCount(SelectedGems,Count),Count>0,
	Action = getGems(SelectedGems, BackGems);
		
	randomGetGems(Gems,Tokens,SelectedGems,BackGems),
	Action = getGems(SelectedGems, BackGems)
	)
	,member(Oponent, Oponents)
	,splendor:stateProxy(Oponent, score, _).

needBonuses(_-RequiredBonuses-_,Bonuses,NeedBonuses):- 	
		subtractGems(RequiredBonuses,Bonuses, BonusGemRemain),
		splendor:nonzeroIndex(BonusGemRemain, 1, NeedBonuses).
		
		
selectCard(NeedBonuses,CanBuyCards,Card):- 	
		member(Card,CanBuyCards),
		card(Card, _-X-_),
		member(X,NeedBonuses),!.
		



closestCardToBuy(Gems,Bonuses,CardIds,RequiredGems):- 
		cardList(CardIds,Cards),!,
		subtractFromCards(Cards,Gems,Bonuses,RequiredGemsList),
		minFromList(RequiredGemsList,RequiredGems).

subtractFromCards([],_,_,[]).
subtractFromCards([L-_-_|Cards],Gems,Bonuses,[RG|RequiredGemsList]):- 
				addGems(Gems,Bonuses,TotalGems),
				removeGems(L,TotalGems,RG),
				subtractFromCards(Cards,Gems,Bonuses,RequiredGemsList).

minFromList([L|List],Gems):- minFromList(List,L,Gems).


				
minFromList([],Min,Min).
minFromList([RG|RequiredGemList],RequiredGems,Min):- 
				gemCount(RG,Count),
				gemCount(RequiredGems,Count2),
				Count<Count2,!,
				minFromList(RequiredGemList,RG,Min);
				minFromList(RequiredGemList,RequiredGems,Min).
				

selectGems(Gems,RequiredGems,SelectedGems,BackGems,Tokens):-
				splendor:isGetGemValid(Gems,RequiredGems,[0,0,0,0,0,0],Tokens),
				SelectedGems = RequiredGems,
				BackGems = [0,0,0,0,0,0].

selectGems(Gems,RequiredGems,SelectedGems,BackGems,Tokens):-
				randomGetGems(Gems, Tokens, RandGems, BackGems),
				addGems(Gems, RandGems, TempGems),
				removeGems(TempGems, BackGems, NewGems),
				removeGems(RequiredGems,NewGems,List),
				gemCount(List,C),
				gemCount(RequiredGems,RC),
				C < RC,!,
				SelectedGems = RandGems.




cardList([],[]).

cardList([I|L1],[C|L2]):- card(I,C),
			  cardList(L1,L2).

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
