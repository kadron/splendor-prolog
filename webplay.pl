:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_json)).
:- use_module(library(www_browser)).

http:location(document_root,	root(.), []).
:- multifile user:file_search_path/2.

user:file_search_path(document_root,	'www').

:- http_handler(document_root(.), serve_files_in_directory(document_root), [prefix]).

:- http_handler(root(.), http_reply_file('www/default.htm', []), []).

:- http_handler(root(hello_world), say_hi, []).

:- http_handler(root(webmethod), webmethod, []).

:- http_handler(root(performAction), performAction, []).

:- http_handler(root(nextStep), nextStep, []).

:- http_handler(root(newGame), newGame, []).

startServer(Port) :-
	http_server(http_dispatch, [port(Port)]),
	atom_concat('http://localhost:', Port, Url),
	www_open_url(Url).

stopServer(Port) :-
	http_stop_server(Port, []).

print_request([]).
print_request([H|T]) :-
        H =.. [Name, Value],
        format('<tr><td>~w<td>~w~n', [Name, Value]),
        print_request(T).

%:- runGame([human, human]).
	
say_hi(Request) :-
	format('Content-type: text/html~n~n', []),
    format('<html>~n', []),
	http_parameters(Request,[ q(Q, [])]),
	format('~w', Q),
    format('<table border=1>~n'),
    print_request(Request),
    format('~n</table>~n'),
    format('</html>~n', []).

:-set_setting(http:cors, [*]).

webmethod(Request) :-
      option(method(options), Request), !,
      cors_enable(Request,
                  [ methods([get,post,delete])
                  ]),
      format('~n').	

webmethod(Request) :-
	cors_enable,
	format('Content-type: text/plain~n~n', []),
	format('{"output": "'),
	format('", ',[]),
	http_parameters(Request,[ q(Q, [])]),
	updateView(Q),
	format('}',[])
	.

updateView(Result) :-
	splendor:openCards(L1,L2,L3),
	findall(A, (card(X, D-_-_), append([[X], D], A)), CardData),
	splendor:tokens(Tokens),
	splendor:isGameEnded(IsEnded),
	findall(Winner, splendor:winner(Winner), Winners),
	findall([NId|Noble], splendor:nobles(NId-Noble-_), NobleList),
	splendor:selectableNobles(SelectableNobles),
	findall([NId2|Noble2], member(NId2-Noble2-_, SelectableNobles), SelectableNobleList),
	format('"response": "~w", "isEnded": ~w, "winners": "~w", "selectableNobles": ~w, "cards": [~w,~w,~w], "cardData": ~w, "tokens": ~w, "nobles": ~w, "players":[', [Result, IsEnded, Winners, SelectableNobleList, L1,L2,L3,CardData, Tokens, NobleList]),
	splendor:getPlayerList(PlayerList),
	length(PlayerList, PlayerCount),
	nth1(PlayerCount, PlayerList, LastPlayer),
	forall( member(Player, PlayerList),
		(
			splendor:player(Player, score, Score),
			splendor:player(Player, gems, Gems),
			splendor:player(Player, bonuses, Bonuses),
			splendor:player(Player, nobleTiles, NobleTiles),
			findall([Nid2|NobleData], member(Nid2-NobleData-_, NobleTiles), PlayerNobleTiles),
			splendor:player(Player, reserves, Reserves),
			splendor:player(Player, module, Module),
			format('{"name": "~w (~w)", "score": ~w, "gems":~w, "bonuses": ~w, "nobleTiles": ~w, "reserves": ~w}', [Player, Module, Score, Gems, Bonuses, PlayerNobleTiles, Reserves]),
			(LastPlayer = Player ; format(','))
		)
	),
	format('], "playerTypes": [',[]),
	findall(
		_,
		(
			tournamentPlayer(Name),
			format('"~w", ', [Name])
		), 
		_
	),
	format('""]')
	.

tournamentPlayer(Name) :-
	directory_files('players',L), 
	member(M, L), 
	file_name_extension(Name, 'pl', M),
	Name \= 'human'.

performAction(Request) :-
    option(method(options), Request), !,
    cors_enable(Request,
                [ methods([get,post,delete])
                ]),
    format('~n').	


performAction(Request) :-
	cors_enable,
	format('Content-type: text/plain~n~n', []),

	format('{"output": "'),
	splendor:verbose(V),
	setVerbose(20),
	http_parameters(Request,[ action(ActionStr, [])]),
	read_term_from_atom(ActionStr, Action, []),
	(
	splendor:doAction(Action),
	WrongAction=false,
	!,
	(splendor:runOneIteration;true),
	(splendor:runOneIteration;true),
	(splendor:runOneIteration;true)
	;
	WrongAction=true
	),
	format('", "wrongAction": ~w, ',[WrongAction]),
	updateView(Action),
	format('}',[]),

	setVerbose(V)
.

nextStep(Request) :-
    option(method(options), Request), !,
    cors_enable(Request,
                [ methods([get,post,delete])
                ]),
    format('~n').	

nextStep(Request) :-
	cors_enable,
	http_parameters(Request,[ ]),

format('Content-type: text/plain~n~n', []),

	format('{"output": "'),
	splendor:verbose(V),
	setVerbose(20),
	(splendor:runOneIteration;true),

	format('", "wrongAction": ~w, ',[false]),
	updateView([]),
	format('}',[]),

	setVerbose(V)
.

newGame(Request) :-
    option(method(options), Request), !,
    cors_enable(Request,
                [ methods([get,post,delete])
                ]),
    format('~n').	


newGame(Request) :-
	cors_enable,
	http_parameters(Request,[ playerCount(PlayerCountStr, []), player1(Player1, []), player2(Player2, []), player3(Player3, []), player4(Player4, [])]),
	atom_number(PlayerCountStr, PlayerCount),
	format('Content-type: text/plain~n~n', []),

	format('{"output": "'),
	splendor:verbose(V),
	setVerbose(20),
	(
		%PlayerCount=2,(runGame([Player1,Player2]);true);
		%PlayerCount=3,(runGame([Player1,Player2,Player3]);true);
		%PlayerCount=4,(runGame([Player1,Player2,Player3,Player4]);true)
		PlayerCount=2,PlayerModules = [Player1,Player2];
		PlayerCount=3,PlayerModules = [Player1,Player2,Player3];
		PlayerCount=4,PlayerModules = [Player1,Player2,Player3,Player4]
	),
	length(PlayerModules, N),
	splendor:initialize(N),
	splendor:initializePlayers(PlayerModules),

	format('", ',[]),

	updateView([]),
	format('}',[]),

	setVerbose(V)
.
