:-module(splendor, [runGame/1, cardDataRaw/13, addGems/3, removeGems/3, subtractGems/3, minusGemTotal/2, gemCount/2, show/3, stateProxy/3, card/2, setVerbose/1, canBuyCard/3, randomGems/4, randomGetGems/4, isGetGemValid/4, runGameBatch/4, doTournament/1, doTournament/2]). 
%, ,minusGem/2

:-dynamic closeCards/3.
:-dynamic openCards/3.
:-dynamic tokens/1.
:-dynamic player/3.
:-dynamic turn/1.
:-dynamic verbose/1.
:-dynamic nobles/1.
:-dynamic selectableNobles/1.

:-  use_module(cardData). 

selectableNobles([]).   %This is used for the case when there are few nobles visit the player at the same turn.

openCards([],[],[]).    %Open visible cards on the board.
availableGems([]).

unshift(X, L, [X|L]).

%close_cards([[],[],[]]).
%card data - A->ID of the card , N1 to N5 - white,blue,green,red,black gems, X->color of the card
%P-> point of the card, L-> deck of the card.
cardData(A, [N1,N2,N3,N4,N5,0]-X-P, L) :- 
	cardDataRaw(L,A,N1,N2,N3,N4,N5,P,_,_,_,_,_),
	(
		cardDataRaw(_,A,_,_,_,_,_,_,1,_,_,_,_),X=1;
		cardDataRaw(_,A,_,_,_,_,_,_,_,1,_,_,_),X=2;
		cardDataRaw(_,A,_,_,_,_,_,_,_,_,1,_,_),X=3;
		cardDataRaw(_,A,_,_,_,_,_,_,_,_,_,1,_),X=4;
		cardDataRaw(_,A,_,_,_,_,_,_,_,_,_,_,1),X=5
	).

% given the card id, finds RequiredGems-BonusColor-Points info of the card
card(A, C) :-
	cardData(A, C, _). 

% finds id, point and colors_needed_to_be_visited_by_this_noble info of all nobles
allNobles(Z) :-
	findall(Id-[N1, N2, N3, N4, N5, 0]-Point, cardDataRawNoble(Id, N1, N2, N3, N4, N5, Point), Z).

% finds the ids of cards in each deck
% initialCards(IdsOfCardsInTheFirstDeck,IdsOfCardsInTheSecondDeck,IdsOfCardsInTheThirdDeck)
initialCards(L1,L2,L3) :-
	findall(X, cardData(X,_-_-_,1), L1),
	findall(X, cardData(X,_-_-_,2), L2),
	findall(X, cardData(X,_-_-_,3), L3).

closeCards(L1,L2,L3) :-
	initialCards(L1,L2,L3).

shuffle(A,B):-
	random_permutation(A,B).

shuffleCards :-
	closeCards(L1, L2, L3),
	shuffle(L1, SL1),
	shuffle(L2, SL2),
	shuffle(L3, SL3),
	retractall(closeCards(_, _, _)),
	assert(closeCards(SL1, SL2, SL3)).

% finds the first N elements of a list
% firstN (N, GivenList, SubList)
firstN(_,[], []).
firstN(N, [H|T], [H|R]) :-
	N1 is N-1,
	(
		N1=0,R=[];
		N1>0,firstN(N1, T, R)
	),!.

%shuffle nobles and select playerCount+1 from the front of the deck as nobles of the game.
shuffleNobles(N) :-
	allNobles(AllNobles),
	shuffle(AllNobles, ShuffledNobles),
	N1 is N+1,
	firstN(N1, ShuffledNobles, SelectedNobles),
	retractall(nobles(_)),
	forall(member(X, SelectedNobles), assert(nobles(X))).

gemCountForPlayerCount(2, 4).
gemCountForPlayerCount(3, 5).
gemCountForPlayerCount(4, 7).

initialize(N) :-
	(retractall(selectableNobles(_))),
	(assert(selectableNobles([]))),
	(retractall(closeCards(_,_,_));true),
	assert(closeCards(L1,L2,L3) :- initialCards(L1,L2,L3)),
	shuffleCards,
	shuffleNobles(N),
	retractall(openCards(_,_,_)),
	assert(openCards([],[],[])),
	dealCards,dealCards,dealCards,dealCards,
	(retractall(tokens(_));true),
	gemCountForPlayerCount(N, C),
	assert(tokens([C,C,C,C,C,5])),
	(retractall(turn(_));true),
	assert(turn(1)).

moveCard(L, [H|T], [H|L], T).

%takes 4 lists ; L-> cards open on the board initially, CL->cards closed on the board initially
%PL-> cards open on the board after operation, PCL->cards closed on the board after operation
%If the L already has 4 cards open or CL has no more cards to give, then these will stay as the open and closed cards.
%Otherwise take a "Card" from CL and put it to PL. PCL is the list which is CL-"Card". New open and closed cards are PL and PCL.
dealCardsSingle(L, CL, PL, PCL) :-
	length(L, Len),
	length(CL, CLen),
	(
		(Len=4;CLen=0),
		PL=L, 
		PCL=CL
		;
		moveCard(L,CL,PL,PCL)
	).

dealCards :- 
	openCards(L1, L2, L3),
	closeCards(CL1, CL2, CL3),
	dealCardsSingle(L1, CL1, PL1, PCL1),
	dealCardsSingle(L2, CL2, PL2, PCL2),
	dealCardsSingle(L3, CL3, PL3, PCL3),
	retractall(openCards(_,_,_)),
	retractall(closeCards(_,_,_)),
	assert(openCards(PL1,PL2,PL3)),
	assert(closeCards(PCL1,PCL2,PCL3)).

% adds player's initial score, bonuses, gems, reserve cards, noble tiles
% additionally uses player's module and its initialize predicate
% for each player name in PlayerModules
initializePlayers(PlayerModules) :-
	length(PlayerModules, PlayerCount),
	PlayerCount > 1, PlayerCount =< 4,
	retractall(player(_,_,_)),
	forall(between(1, PlayerCount, PlayerNo),
		(
			nth1(PlayerNo, PlayerModules, PlayerModule),
			atom_concat(player, PlayerNo, Player),
			assert(player(Player, score, 0)),
			/* % noble tile selection test
				(
				PlayerNo=1,assert(player(Player, bonuses, [3,3,3,3,3,0]));
				PlayerNo>1,assert(player(Player, bonuses, [0,0,0,0,0,0]))
				),
			*/
			assert(player(Player, bonuses, [0,0,0,0,0,0])),

			assert(player(Player, gems, [0,0,0,0,0,0])),
			assert(player(Player, reserves, [])),
			assert(player(Player, nobleTiles, [])),
			assert(player(Player, module, PlayerModule)),
			atom_concat('players/', PlayerModule, PlayerModuleFile),
			use_module(PlayerModuleFile),
			PlayerModule:initialize(Player, PlayerCount)
		)
	).

stateProxy(game, cards, L) :-
	openCards(L1, L2, L3),
	append([L1,L2,L3], L).

stateProxy(game, closeCards, [L1, L2, L3]) :-
	closeCards(TL1, TL2, TL3),
	shuffle(TL1, L1),
	shuffle(TL2, L2),
	shuffle(TL3, L3)
	.

stateProxy(game, tokens, Tokens) :-
	tokens(Tokens).

stateProxy(X, Y, Z) :-
	player(X, Y, Z).

stateProxy(game, nobles, L) :-
	findall(X, nobles(X), L).

runGame(PlayerModules) :-
	length(PlayerModules, N),
	initialize(N),
	initializePlayers(PlayerModules),
	!,
		gameStep(0),
	!,
	false
	.

runGameBatchOne([P1, P2], Count, P1WinCount, P2WinCount) :- 
	show(-10,'~w vs ~w (~w remaining games)                           \r', [P1, P2, Count]),
	(runGame([P1, P2]);true),
	!
	,
	(
		(winner(player1), P1WinCountNew is 1, P2WinCountNew is 0;true),!,
		(winner(player2), P1WinCountNew is 0, P2WinCountNew is 1;true),!,
		(\+winner(player1), \+winner(player2), P1WinCountNew is 0, P2WinCountNew is 0 ;true),!
	),
	Count1 is Count-1,
	!
	,
	(
		Count1>0,
		runGameBatchOne([P1, P2], Count1, P1WinCountRest, P2WinCountRest),
		!
		,
		P1WinCount is P1WinCountNew + P1WinCountRest,
		P2WinCount is P2WinCountNew + P2WinCountRest
		;
		Count1=0,
		P1WinCount is P1WinCountNew,
		P2WinCount is P2WinCountNew
	),!.

% perform 2*Count batch games agains players P1 & P2 by swaping sides
runGameBatch([P1, P2], Count, P1WinCount, P2WinCount) :-
	verbose(Verbose),
	setVerbose(-10),
	runGameBatchOne([P1,P2], Count, P1WinCount1, P2WinCount1),!,
	runGameBatchOne([P2,P1], Count, P2WinCount2, P1WinCount2),!,
	P1WinCount is P1WinCount1+P1WinCount2,
	P2WinCount is P2WinCount1+P2WinCount2,
	setVerbose(Verbose)
.

tournamentPlayer(Name) :-
	directory_files('players',L), 
	member(M, L), 
	file_name_extension(Name, 'pl', M),
	Name \= 'human',
	Name \= 'webPlayer'.

doTournament(Count) :-
	findall(
		Name,
		tournamentPlayer(Name), 
		Players
	),
	doTournament(Players, Count)
	.

doTournament(Players, Count) :-
	show(-1, 'Tournament starting with players: ~w~n~n',[Players]),
	!,
	findall(
		P1-P2-P1Win-P2Win,
		(
			member(P1, Players),
			member(P2, Players),
			P1 @< P2,
			runGameBatch([P1, P2], Count, P1Win, P2Win),
			show(-10,'~w: ~w - ~w: ~w                                              \n', [P1, P1Win, P2, P2Win])
		),
		Results
	),
	%show(-1, 'Tournament completed: ~w~n', [Results]),
	tournamentResults(Players, Results)
	.

tournamentResults(Players, Results) :-
	findall(
		Player-Point,
		(
			(member(Player-_-W1-W2, Results);member(_-Player-W2-W1, Results)),
			(
				(W1>W2,Point=1);
				(W1=W2,Point=0.5);
				(W1<W2,Point=0)
			)
		),
		PlayerPoints
	),
	%show(-1, 'Player points: ~w~n', [PlayerPoints]),
	findall(
		Player-Score,
		(
			member(Player, Players),
			findall(
				Point,
				member(Player-Point, PlayerPoints),
				Points
			),
			sum_list(Points, Score)
		),
		PlayerScores
	),
	%show(-1, 'Player scores: ~w~n', [PlayerScores]),
	sort(2, @>=, PlayerScores, PlayerScoresSorted),
	show(-1, '~n~nTournament result table: ~n',[]),
	show(-1,     '======================== ~n',[]),
	forall(
		member(Player-Score, PlayerScoresSorted),
		show(-1, '~w\t: ~w~n',[Player, Score])
	),
	!
	.


runOneIteration :- 
	currentPlayer(Player),
	oponent(Oponents),
	getPlayerModule(Player, Module),
	!,
	
	Module:decideAction(Player, Oponents, stateProxy, Action),
	doAction(Action)
.

getNoble(Player, Id) :-
	selectableNobles(L),
	member(Id-NobleData-C, L),
	getNobleCard(Player, Id-NobleData-C)
	.

% getNoble action
doAction(Action) :-
	currentPlayer(Player),
	Action = getNoble(NobleId),
	getNoble(Player, NobleId)
	.

% all other actions
doAction(Action) :-
	currentPlayer(Player),

	% if the player should select one noble tile, it cannot perform any other action
	selectableNobles(NobleGets),
	length(NobleGets, NobleCount),
	NobleCount=0,

	show(1, '~w chooses action: ~w ~n',[Player, Action]),
	(	%The case where the player wants to get gems.
		Action = getGems(TryGems,TryBackGems),    %Gems are players' coins. Tokens are board coins.
		tokens(Tokens),
		retract(player(Player, gems, CurrentGems)),

		show(20,'~n~nGetGem: ~w ~w ~w~n~n', [TryGems, TryBackGems, Tokens]),

		(
			% check if getGem action is valid
			isGetGemValid(CurrentGems, TryGems, TryBackGems, Tokens),show(20, 'Gems valid~n', []),Gems=TryGems,BackGems=TryBackGems;

			% if it is not replace it with random get gem action
			\+isGetGemValid(CurrentGems, TryGems, TryBackGems, Tokens),show(5, 'Gems INVALID: ~w ~w ~w ~w!!~n', [TryGems,TryBackGems,Tokens,CurrentGems]), randomGetGems(CurrentGems, Tokens, Gems, BackGems),show(5, 'RAMDOM GEMS SELECTED!!: ~w ~w~n', [Gems, BackGems])
		),

		addGems(CurrentGems, Gems, TempGems),
		removeGems(TempGems, BackGems, NewGems),
		assert(player(Player, gems, NewGems)),
		retract(tokens(_)),
		removeGems(Tokens, Gems, TempTokens),
		addGems(TempTokens, BackGems, NewTokens2),
		assert(tokens(NewTokens2))
		;
		Action = buyCard(CardId),
		playerBuysCard(Player,CardId)
		;
		Action = reserveCard(CardId, BackGems),
		playerReservesCard(Player, CardId, BackGems)
		;
		Action = reserveCardFromDeck(DeckId, BackGems),
		playerReservesCardFromDeck(Player, DeckId, BackGems)
	),
	nextTurn
	.

playerCount(N) :-
	findall(PlayerScore, player(_, score, PlayerScore), PlayerScores),
	length(PlayerScores, N).

nextTurn :- 
	retract(turn(X)),
	playerCount(PlayerCount),
	Y is mod(X,PlayerCount)+1,
	assert(turn(Y)).

gameStep(N) :-
	M is N+1,
	show(1, 'Step ~w: ', [M]),
	runOneIteration,
	(
		isGameEnded,
		show(10, 'Game ended~n', []),
		findall(Player-Score, player(Player, score, Score), Scores),
		show(0, 'Scores: ~w~n',[Scores]),
		winner(WinnerPlayer),
		show(0, 'Winner: ~w~n', [WinnerPlayer])
		;
		M=500,
		show(-1, 'Max steps executed~n', [])
		;
		gameStep(M))
	.

getPlayerModule(Player, Module) :-
	player(Player, module, Module).

getPlayerList(PlayerList) :-
	setof(Player, A^B^player(Player, A, B), PlayerList).

playerState(Player, X, Y) :-
	player(Player, X, Y).

currentPlayer(Player) :-
	turn(T),
	getPlayerList(PlayerList),
	nth1(T, PlayerList, Player).

oponent(Oponents) :- 
	currentPlayer(Player),
	getPlayerList(PlayerList),
	findall(Oponent, (member(Oponent, PlayerList),\+Oponent = Player),Oponents).

isGameEnded :- 
	player(_, score, A),
	A >= 15,
	turn(1).

isGameEnded(true) :-
	isGameEnded.

isGameEnded(false) :-
	\+ isGameEnded.

winner(Player) :-
	player(Player, score, Score),
	Score >= 15,
	\+greaterPlayer(Player, Score).

greaterPlayer(Player, Score) :-
	player(Other, score, OtherScore),
	\+Player = Other,
	show(70, 'Player/Other: ~w ~w~n', [Player, Other]),
	(
		OtherScore > Score
		;
		OtherScore = Score,
		player(Other, bonuses, OtherBonus),
		player(Player, bonuses, PlayerBonus),
		gemCount(OtherBonus, OtherBonusCount),
		gemCount(PlayerBonus, PlayerBonusCount),
		OtherBonusCount<PlayerBonusCount
	).


addGemsA([], [], []).
addGemsA([H1|T1], [H2|T2], [H3|T3]) :-
    H3 is H1 + H2,
    addGems(T1, T2, T3).

addGems([A1,A2,A3,A4,A5,A6], [B1,B2,B3,B4,B5,B6], [C1,C2,C3,C4,C5,C6]) :- 
	C1 is A1+B1,
	C2 is A2+B2,
	C3 is A3+B3,
	C4 is A4+B4,
	C5 is A5+B5,
	C6 is A6+B6.

subtractGems([A1,A2,A3,A4,A5,A6], [B1,B2,B3,B4,B5,B6], [C1,C2,C3,C4,C5,C6]) :- 
	C1 is A1-B1,
	C2 is A2-B2,
	C3 is A3-B3,
	C4 is A4-B4,
	C5 is A5-B5,
	C6 is A6-B6.

removeGems([A1,A2,A3,A4,A5,A6], [B1,B2,B3,B4,B5,B6], [C1,C2,C3,C4,C5,C6]) :- 
	C1 is max(A1-B1,0),
	C2 is max(A2-B2,0),
	C3 is max(A3-B3,0),
	C4 is max(A4-B4,0),
	C5 is max(A5-B5,0),
	C6 is max(A6-B6,0).

minusGemTotal([N1,N2,N3,N4,N5,N6], T) :-
	T is -(min(N1,0)+min(N2,0)+min(N3,0)+min(N4,0)+min(N5,0)+min(N6,0)).

bonusGem(1, [1,0,0,0,0,0]).
bonusGem(2, [0,1,0,0,0,0]).
bonusGem(3, [0,0,1,0,0,0]).
bonusGem(4, [0,0,0,1,0,0]).
bonusGem(5, [0,0,0,0,1,0]).

gemCount([N1,N2,N3,N4,N5,N6], N) :-
	N is N1+N2+N3+N4+N5+N6.

% removes a card from open cards or reserves. Remove card is used when a card is bought. We can only buy cards from
%the open cards on board or our reserve.
removeCard(Player, CardId) :-
	player(Player, reserves, Reserves),
	openCards(L1, L2, L3),
	append([L1,L2,L3,Reserves], L),

	member(CardId, L),
	subtract(L1, [CardId], NL1),
	subtract(L2, [CardId], NL2),
	subtract(L3, [CardId], NL3),
	subtract(Reserves, [CardId], NewReserves),
	
	retract(player(Player, reserves, _)),
	assert(player(Player, reserves, NewReserves)),
	(retractall(openCards(_,_,_));true),
	assert(openCards(NL1, NL2, NL3))
	.

% removes a card from deck. used for reserving a card from deck
removeCardFromDeck(CardId) :-
	closeCards(L1, L2, L3),
	append([L1,L2,L3], L),
	member(CardId, L),
	subtract(L1, [CardId], NL1),
	subtract(L2, [CardId], NL2),
	subtract(L3, [CardId], NL3),
	
	(retractall(closeCards(_,_,_));true),
	assert(closeCards(NL1, NL2, NL3))
	.

playerReservesCard(Player, CardId, BackGems) :- 
	playerReservesCard(Player, CardId, BackGems, false).

playerReservesCard(Player, CardId, BackGems, FromDeck) :-
	player(Player, reserves, Reserves),
	length(Reserves, ReserveCount),
	ReserveCount < 3,
	(
		\+FromDeck,removeCard(Player, CardId);
		FromDeck,removeCardFromDeck(CardId)
	),
	tokens(Tokens),
	nth1(6, Tokens, GoldCount),
	(
		GoldCount>0,
		retract(player(Player, gems, Gems)),
		retract(tokens(_)),
		removeGems(Tokens, [0,0,0,0,0,1], NewTokens),
		addGems(Gems, [0,0,0,0,0,1], NewGems),
		totalGem(NewGems, NewGemsTotal), 
		show(20, 'Checking to give back: ~w ~w ~n', [NewGems, NewGemsTotal]),
		(
			NewGemsTotal=<10,
			NewNewGems = NewGems,
			NewNewTokens = NewTokens
			;
			NewGemsTotal>10, 
			show(20, 'Reserve causes more then 10 gems~n', []),
			subtractGems(NewGems, BackGems, NewNewGems),
			totalGem(NewNewGems, NewNewGemsTotal),
			minGem(NewNewGems, NewNewMinGem),
			NewNewGemsTotal = 10,
			NewNewMinGem>=0,
			addGems(NewTokens, BackGems, NewNewTokens),
			show(20, 'Giving back gems: ~w ~w ~w ~w ~w ~w ~n', [NewGems, BackGems, NewNewGems, NewTokens, BackGems, NewNewTokens])
		),
		assert(tokens(NewNewTokens)),
		assert(player(Player, gems, NewNewGems))
		;
		GoldCount=<0
	),
	retract(player(Player, reserves, _)),
	assert(player(Player, reserves, [CardId|Reserves])),
	dealCards,
	!
	.

% reserves the first card of a deck (deck is chosen by the player)  
playerReservesCardFromDeck(Player, DeckId, BackGems) :-
	(
		DeckId = 1, closeCards([CardId|_], _, _);
		DeckId = 2, closeCards(_, [CardId|_], _);
		DeckId = 3, closeCards(_, _, [CardId|_])
	),
	playerReservesCard(Player, CardId, BackGems, true),!
	.

playerBuysCard(Player, CardId) :- 
	player(Player, reserves, PlayerReserves),
	openCards(L1, L2, L3),
	append([L1,L2,L3, PlayerReserves], AllAvailableCards),  %All card that can be bought if there are enough gems.
	show(20, 'Available cards:~w ~w ~w~n', [CardId, PlayerReserves, AllAvailableCards, CardId]),
	member(CardId, AllAvailableCards),
	!,
	card(CardId, RequiredGems-BonusColor-Points),
	player(Player, gems, Gems),
	player(Player, bonuses, Bonuses),

	removeGems(RequiredGems, Bonuses, RequiredMinusBonus),	%Subtract card bonuses from the required gems for buying the card. 
	subtractGems(Gems, RequiredMinusBonus, RemainingGems),  %Subtract the remanining gems from current gems. 

	minusGemTotal(RemainingGems, MinusTotal),		%If the remaining gems contain minus numbers,
	nth1(6, Gems, GoldCount),			        %Look at the gold gems of the player.
	MinusTotal =< GoldCount,				%If it has enough gold gems to compensate then remove the gems.

	removeGems(Gems, RequiredMinusBonus, RemainingGems1),
	removeGems(RemainingGems1, [0,0,0,0,0,MinusTotal], RemainingGems2),

	removeCard(Player, CardId),
	retract(player(Player, gems, _)),
	retract(player(Player, bonuses, _)),

	assert(player(Player, gems, RemainingGems2)),

	retract(tokens(Tokens)),
	removeGems(RequiredMinusBonus, Gems, PaidByGoldenGems),	  %we remove gems from requiredMinusBonus to see how many gems are
	addGems(Tokens, RequiredMinusBonus, NewTokens),		  %taken from gold pile. removeGems will have 0s when the subtraction
	subtractGems(NewTokens, PaidByGoldenGems, NewTokens2),	  %results in a negative number so when we subtract them we do not have
	addGems(NewTokens2, [0,0,0,0,0,MinusTotal], NewTokens3),  %to worry about adding gems to the board tokens more than necessary.	
	assert(tokens(NewTokens3)),

	bonusGem(BonusColor, BonusGem),
	addGems(Bonuses, BonusGem, NewBonuses),
	assert(player(Player, bonuses, NewBonuses)),

	retract(player(Player, score, Score)),
	NewScore is Score + Points,
	assert(player(Player, score, NewScore)),

	dealCards,						 %put a card in place for the bought card

	canGetNobles(NewBonuses, NobleGets),			%The case where we have nobles who want to visit us
	
	length(NobleGets, NobleGetCount),
	(
		NobleGetCount = 0				%If there are none, continue.
		;
		NobleGetCount = 1,			        %If there is one then add it to the player nobles.   
		NobleGets = [NobleCard],
		getNobleCard(Player, NobleCard)
		;
		NobleGetCount > 1,			        %If there are more than 1, prompt player to select one.
		retractall(selectableNobles(_)),
		assert(selectableNobles(NobleGets)),
		show(5, 'Can get multiple nobles, should decide : ~w~n', [NobleGets]),
		!,
		getPlayerModule(Player, Module),
		Module:selectNoble(NobleGets, SelectedNoble),
		show(5, '~w chooses noble card: ~w~n', [Player, SelectedNoble]),
		getNobleCard(Player, SelectedNoble)
	),

	show(20, 'Getted noble tiles: ~w~n', [NobleGets]),
	!
	.

getNobleCard(Player, NobleCard) :-
	show(5, '~w receives noble card: ~w~n', [Player, NobleCard]),

	NobleCard = Nid-X2-C2, 
	retract(nobles(Nid-X2-C2)),
	retract(player(Player, score, Score2)),
	NewScore2 is Score2 + C2,
	assert(player(Player, score, NewScore2)),

	retract(player(Player, nobleTiles, CurrentNobles)),
	append([CurrentNobles, [Nid-X2-C2]], NewPlayerNobles),
	assert(player(Player, nobleTiles, NewPlayerNobles)),
	retractall(selectableNobles(_)),
	assert(selectableNobles([]))
	.

canGetNobles(Bonusses, NobleGets) :-
	findall(
		NId-X-C, 
		(
			nobles(NId-X-C),
			subtractGems(Bonusses, X, BonusGemRemain),
			splendor:minGem(BonusGemRemain, MinC),
			MinC >= 0
		),
		NobleGets
	).

openCardsFlat(L) :-
	openCards(L1, L2, L3),
	append([L1,L2,L3], L).

verbose(10).

setVerbose(N) :-
	retractall(verbose(_)),
	assert(verbose(N)).

show(V, A, B) :-
	verbose(T),
	(T>=V,format(A,B);T<V).

testAll :-
	forall(between(1, 100, N),
		(
			set_random(seed(N)), 
			format('~w~n',[N]), 
			(runGame([randomPlayer, randomPlayer]);true)
		)
	).

/************************************ isGetGemValid ************************************/

% Check if the player currently having Curr gems, can take X gems and give XBack gems provided that C gems are available
% all are lists of length 6
isGetGemValid(Curr, X, XBack, C) :-
	isGetGemValid1(X),
	isGetGemValid2(X, C),
	isGetGemValid3(X),
	isGetGemValid4(X,C),
	!,
	totalGem(Curr, T1),
	totalGem(X, T2),
	totalGem(XBack, T3),
	T is T1+T2-T3,
	T=<10,
	T3>=0,
	addGems(Curr, X, New1),
	subtractGems(New1,XBack,New2),
	minGem(New2, MinC),
	MinC >= 0,
	!
	.

% number of gems to get must be <= 3
isGetGemValid1(X) :- 
	totalGem(X, A),
	A=<3,!.

% there must be enough gems on the board x_i < C_i
isGetGemValid2([],[]).
isGetGemValid2([_],[_]).
isGetGemValid2([X1|XR], [C1|CR]) :-
	X1 =< C1,
	(
		X1<2;C1>=4
	),
	isGetGemValid2(XR, CR),!.

% if 2 gems are of the same color, all other gems must be zero
isGetGemValid3([]).
isGetGemValid3([0]).
isGetGemValid3([H|T]) :- 
	(
		(H=0,isGetGemValid3(T));
		(H=1,maxGem(T,A), A=<1);
		(H=2,maxGem(T,A), A=0);
		(H>2,false)
	),!.

% if there are at least 3 available gems of different color and the player selects no 2 gems, it must select 3 different color gems
% else if there are 2 available gems of different color and a color with at least 4 gems, the player should select two of the same color
% else the player needs to get one of each available gems
isGetGemValid4(X,C) :-
	nonzero(C, A),
	nonzero(X, B),
	maxGem(X, M),
	maxGem(C, MC),
	(
		(
			M>1,
			B=1
		);
		(
			M=<1,
			(
				(A>=3,B=3);
				(A=2,A=B);
				(A=1,MC<4)
			)
		)
	),!
	.

% number of nonzero (actually greater than zero) gems in list
nonzero([_], 0).
nonzero([H|T], N) :-
	nonzero(T, A),
	!
	,
	(
		(H>0,N is A+1,!);
		(H=0,N is A,!)
	).

% total number of gems in list
totalGem([], 0).
%totalGem([_], 0).  %ersin - burasi eskiden boyleydi. neden acaba?
totalGem([X], X).
totalGem([H|L], T) :-
	totalGem(L, A),
	T is A+H.

% the max count of gems in the list
maxGem([], 0).
maxGem([_], 0).
maxGem([H|L], T) :-
	maxGem(L, A),
	max(H,A,T).

% the min count of gems in the list
minGem([], 9999).
minGem([H|L], T) :-
	minGem(L, A),
	min(H,A,T),!.

% C = max(A,B)
max(A,B,C) :- 
	(A>=B,C is A,!);(C is B),!.

% C = min(A,B)
min(A,B,C) :- 
	(A=<B,C is A,!);(C is B),!.

/************************************ canBuyCard, randomGems ************************************/

canBuyCard(Gems, Bonuses, CardId) :-
	card(CardId, NeededGems-_-_),
	addGems(Gems, Bonuses, TotalGems),
	subtractGems(TotalGems, NeededGems, RemainingGems),
	minusGemTotal(RemainingGems, MinusTotal),
	%show(40, 'Minus total: ~w~n', [MinusTotal]),
	nth1(6, Gems, GoldCount),
	MinusTotal =< GoldCount
	.
%gets random gems from gems and also keeps the excessive gems in backGems. If there are excessive gems after adding the newly
%selected gems, then the backGems are selected also randomly from NewGems.
randomGetGems(Gems, Tokens, RandGems, BackGems) :-
	randomGems(Tokens, 3, true, RandGems),
	addGems(Gems, RandGems, NewGems),
	gemCount(NewGems, GemCount),
	ExcessGemCount is GemCount -10,
	(ExcessGemCount=<0,BackGems=[0,0,0,0,0,0];ExcessGemCount>0,randomGems(NewGems, ExcessGemCount, false, BackGems)).

%RandGems is random set of gems from BaseTokens.
randomGems(BaseTokens, N, HasLimit, RandGems) :-
	removeGems(BaseTokens, [0,0,0,0,0,1000], Tokens), %This is for ensuring no gold tokens are taken from the board.
	randomGems2(Tokens, N, HasLimit, [], RandGems),!.

%gets a list and gives a list indicating which places on the initial list are nonzero. The gems are given indexes from 1 to 6.
%so if a list returns as [1,3,5] then that means there are only 3 piles of tokens on the board and they are white,green,black.
nonzeroIndex([], _, []).
nonzeroIndex([H|T], I, A) :-
	J is I+1,
	nonzeroIndex(T, J, B),
	(H>0,A = [I|B];H=<0,A = B).

gemByIdx(1, [1, 0, 0, 0, 0, 0]).
gemByIdx(2, [0, 1, 0, 0, 0, 0]).
gemByIdx(3, [0, 0, 1, 0, 0, 0]).
gemByIdx(4, [0, 0, 0, 1, 0, 0]).
gemByIdx(5, [0, 0, 0, 0, 1, 0]).
gemByIdx(6, [0, 0, 0, 0, 0, 1]).

multGem([], _, []).

multGem([N], K, [M]) :-
	M is K*N.

multGem([H|N], K, [X|M]) :-
	multGem([H], K, [X]),
	multGem(N, K, M),
	!.

randomGems2(_, 0, _, _, [0,0,0,0,0,0]). %base case where we have the amount of gems we need.
randomGems2([0,0,0,0,0,0], _, _, _, [0,0,0,0,0,0]).	%base case where we do not have any more gem to take.
randomGems2(Tokens, N, HasLimit, Selected, A) :-  
	nonzeroIndex(Tokens, 1, NonZero),
	length(NonZero, NonZeroLength),
	NonZeroLength1 is NonZeroLength+1, %to use in the random/3 we need to increase 1 because upper limit is not inclusive.
	repeat,				    %this will keep repeating the following predicates until they succeed.
	random(1, NonZeroLength1, NonZeroIdx),
	nth1(NonZeroIdx, NonZero, Idx),    %get the id of the tokens we are going to take.
	nth1(Idx, Tokens, Count),	   %get the count of the selected token.
	Count>0,			   %This is checking that token is indeed nonzero. It was checked before.
	gemByIdx(Idx, A1),
	(				  %The case where we take 2 gems.
		HasLimit,		  %HasLimit is true when we are getting tokens from the board. It is false when we are
		N=3,			  %returning excessive gems. 
		Count>=4,		  %If the selected colour has at least 4 tokens then we can take 2 from them.
		(NonZeroLength=<2;random(1, 3, 2)),  %If there are less than 3 piles of tokens on the board  this is legal.
		multGem(A1, 2, A)		     %Also random(1,3,2) is always true so the general case where there are 3 or more
		;				     %piles of tokens. Multiply A1 which is a vector with 0's and one 1 where the 1 indicates the colour of the gem.		
		(  %The case where we take different colour gems.
			HasLimit,multGem(A1, 1000, A1X)   %If we are taking gems from board, this ensures that we are not taking the same
			;				  %colour gem again.
			\+HasLimit,multGem(A1, 1, A1X) 	  %There is no such concern for giving back excessive gems.
		),
		removeGems(Tokens, A1X, Tokens2),
		N2 is N-1,				%The count of coins are decreased by 1. 
		randomGems2(Tokens2, N2, HasLimit, [Idx|Selected], A2),	%The selected gem is added to Selected.
		addGems(A2, A1, A)	%After the recursion comes back, add the selected gem to the all previous gems and put to A.
	),
	!
	.
