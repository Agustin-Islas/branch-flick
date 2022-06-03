:- module(proylcc, 
	[  
		initCell/1,
		adjacentC/1,
		setInit/2,
		stackPlays/1,
		setAdjacent/2,
		checkEnd/1,
		flickAdjacents/3,
		findAdjacentC/2,
		pushStackPlays/2,
		resetStackPlays/0,
        captured/1,
        searchCombinations/2,
		bestPlay/1,
		maxCaptureds/1,
		resetHelp/0
	]).

:-use_module(library(lists)).

:-dynamic adjacentC/1.

:-dynamic initCell/1.

:-dynamic stackPlays/1.

:-dynamic captured/1.

:-dynamic colors/1.
:-dynamic maxCaptureds/1.
:-dynamic bestPlay/1.

colors([r, v, p, g, b, y]).

% Almacena la longuitud de la sucuencia de colores con celdas capturadas.
maxCaptureds(0).

% Almacena una lista con la secuencia de colores a retornar.
bestPlay([[]]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Almacena la celda inicial del juego.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
initCell([0, 0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Almacena las celdas adjacentes capturadas hasta el momento.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
adjacentC([[0, 0]]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pushStackPlays(+Color, -Plays)
% Plays es la pila de plays actuales a la que se le inserto Color como head.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pushStackPlays(Color, [Color|S]):-
    stackPlays(S),
    S \= [],
    retract(stackPlays(S)),
	assert(stackPlays([Color|S])).

pushStackPlays(Color, [Color]):-
	assert(stackPlays([Color])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% resetStackPlays()
% vacia la pila de plays actuales.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
resetStackPlays():-
    retract(stackPlays(_S)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% setInit(+X, +Y)
% Reemplaza la celda inicial actual por la celda (X, Y) recibida por parametro,
% luego llama a setAdjacent(+X, +Y), con la celda recibida.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
setInit(X,Y):-
	checkPos(X,Y),
	initCell(Cell),
	retract(initCell(Cell)),
	assert(initCell([X,Y])),
	setAdjacent(X,Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkPos(+X, +Y)
% verifica que la celda de coordenadas (X, Y) pertenezca a la grilla.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkPos(X,Y):-
	X > -1, X < 15,
	Y > -1, Y < 15.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% setAdjacent(+X, +Y)
% reemplaza las adjacencias actuales por la lista que contiene [X, Y].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
setAdjacent(X, Y):-
	retract(adjacentC(_L)),
    assert(adjacentC([[X,Y]])).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% findAdjacentC(+Grid, +Color)
% Recibe una grilla Grid y un color Color y busca y agrega nuevos adjacentes
% a adjacentC.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
findAdjacentC(Grid, Color):-
	adjacentC(List),
	findAux(Grid, List, Color).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% findAux(+Grid, +List, +Color)
% recibe una grilla Grid una lista List de adjacentes actuales y un color Color
% y busca recursivamente nuevos adjacentes y los agrega a adjacentC.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
findAux(Grid, List, Color):-
	List = [L|Ls],
	L = [X,Y],
	adjacents(Grid, X, Y, Color),
	findAux(Grid, Ls, Color).
findAux(_Grid, [], _Color).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% capturedCells(-Captured)
% Captured es la cantidad de celdas adjacentes capturadas hasta el momento.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
capturedCells(Captured):-
	adjacentC(List),
	list_length(Captured, List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% list_length(-L, +List)
% L es el resultado de buscar la longuitud de la lista List.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
list_length(0, []).
list_length(L, [_H|T]):-
	list_length(N, T),
	L is N+1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkEnd(-End)
% Verifican si todas las celdas de la grilla fueron capturadas,
% End retorna true si esa condicion se cumple y false en caso contrario.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkEnd(true):-
	capturedCells(Captured),
	Captured == 196.
checkEnd(false).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% flickAdjacents(+Grid, +Color, -FGrid)
% FGrid es el resultado de aplicar 'flick' a Color a todas las celdas
% adjacentes capturadas hasta el momento en Grid.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
flickAdjacents(Grid, Color, FGrid):-
	adjacentC(List),
	flickAux(Grid, List, Color, FGrid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% flickAux(+Grid, +List, +Color, -FGrid)
% recibe la lista List de posiciones capturadas y una por una les aplica 
% 'flick' a Color. Luego retorna en FGrid la grilla Grid actualizada.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
flickAux(Grid, List, Color, FGrid):-
	List = [L|Ls],
	L = [X,Y],
	flickAux(Grid, Ls, Color, FGridAux),
	flick(FGridAux, X, Y, Color, FGrid).
flickAux(Grid, [], _Color, Grid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% adjacent(+Grid, +X, +Y, +Color)
% Recibe una posicion (X, Y) de Grid y busca sus AdjacentC* y los agrega a 
% AdjacentC, si es que existen.
% Una celda C es AdjacentC* a (X, Y) si es del mismo Color y si C es adjacente
% a (X, Y) o existe una celda C* del mismo color que (X, Y) y C tal que (X, Y)
% es adyacente a C* y recursivamente C* es adyacenteC* a C.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
adjacents(Grid, X, Y, Color):-
	X1 is X-1, X2 is X+1,
	Y1 is Y-1, Y2 is Y+1,
	isAdj(Grid, X1, Y, Color),
	isAdj(Grid, X2, Y, Color),
	isAdj(Grid, X, Y1, Color),
	isAdj(Grid, X, Y2, Color).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% isAdj(+Grid, +X, +Y, +Color)
% Si el Elemento Elem ubicado en la coordenada (X, Y) es igual a Color y (X, Y)
% no pertenece a la lista adjacentC, agrega el par [X, Y] a adjacentC y llama 
% a la función adjacent(+Grid, +X, +Y, +Color).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
isAdj(Grid, X, Y, Color):-
	getPos(Grid, X, Y, Elem),
	Elem == Color,
	adjacentC(L),
	not(member([X,Y], L)),
	retract(adjacentC(L)), union(L, [[X,Y]], NewL),
	assert(adjacentC(NewL)),
	adjacents(Grid, X, Y, Color).

isAdj(_Grid, _X, _Y, _Color).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% getPos(+Grid, +X, +Y, -Elem)
% Elem es el elemento ubicado en la coordenada (X, Y) de Grid.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getPos(Grid, X, Y, Elem):-
	nth0(X,Grid,Column),
	nth0(Y,Column,Elem).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% flick(+Grid, +X, +Y, +Color, -FGrid)
% FGrid es el resultado de reemplazar el elemento Elem ubicado en la coordenada 
% (X, Y) de la grilla Grid por Color.
% Si Elem es igual a Color, no se produce cambio.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
flick(Grid, X, Y, Color, FGrid):-
	nth0(X, Grid, Column),
	nth0(Y, Column, Elem),
	Color \= Elem,
	setPos(Y, Color, Column, FColumn),
	setPos(X, FColumn, Grid, FGrid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% setPos(+Pos, +NewElem, +List, -NewList)
% NewList es el resultado de haber insertado NewElem en la posicion Pos de 
% List.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
setPos(Pos, NewElem, [H|T], [H|R]):-
	Pos > -1, PosN is Pos-1, setPos(PosN, NewElem, T, R).
setPos(0, NewElem, [_Elem|T], [NewElem|T]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PROYECTO 2 - HELP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resetHelp():-
    retract(maxCaptureds(_C)),
    assert(maxCaptureds(0)),
    retract(bestPlay(_L)),
    assert(bestPlay([[]])).

/* -L: bestPlay, -Cant: Long L*/
searchCombinations(Depth, Grid):-
    colors(Colors),
    aux(Colors, Depth, Grid).

aux([C|Cs], Depth, Grid):-
    findall(List,
            flickHelp(C, Depth, List),
            AllLists),
    findMaxCaptureds(Grid, AllLists, L, CantCaptureds),
    actualizeBestPlay(L, CantCaptureds),
    aux(Cs, Depth, Grid),
    !.

aux([], _Depth, _Grid).

flickHelp(_Color, 0, []):-!.
flickHelp(Color, Depth, [r|Ls]):-
    Color \= r,
    D is Depth - 1,
    flickHelp(r, D, Ls).
flickHelp(Color, Depth, [v|Ls]):-
    Color \= v,
    D is Depth - 1,
    flickHelp(v, D, Ls).
flickHelp(Color, Depth, [y|Ls]):-
    Color \= y,
    D is Depth - 1,
    flickHelp(y, D, Ls).
flickHelp(Color, Depth, [g|Ls]):-
    Color \= g,
    D is Depth - 1,
    flickHelp(g, D, Ls).
flickHelp(Color, Depth, [b|Ls]):-
    Color \= b,
    D is Depth - 1,
    flickHelp(b, D, Ls).
flickHelp(Color, Depth, [p|Ls]):-
    Color \= p,
    D is Depth - 1,
    flickHelp(p, D, Ls).

findMaxCaptureds(Grid, [L|Ls], ListLs, CantLs):-
    findMaxCaptureds(Grid, Ls, ListLs, CantLs),
    findCaptureds(Grid, L, Cant),
    CantLs >= Cant.

findMaxCaptureds(Grid, [L|_Ls], L, Cant):-
    findCaptureds(Grid, L, Cant).

findCaptureds(Grid, [C|Cs], Cant):-
   initCell([X, Y]),
   flickHandler(C, X, Y, Grid, FGrid),
   findCaptureds(FGrid, Cs, Cant).
findCaptureds(Grid, [], Cant):-
    initCell([X, Y]),
    adyCStar([X, Y], Grid, Res),
    length(Res, Cant),
    !.

flickHandler(Color, X, Y, Grid, FGrid):-
   adyCStar([X, Y], Grid, Res),
   flickAdj(Grid, Color, Res, FGrid).

flickAdj(Grid, Color, [A|Adjs], FGrid):-
    A = [X, Y],
    flickP2(Grid, X, Y, Color, AuxGrid),
    flickAdj(AuxGrid, Color, Adjs, FGrid).

flickAdj(Grid, _Color, [], Grid).

actualizeBestPlay(List, CantCaptureds):-
    maxCaptureds(Cant),
    CantCaptureds > Cant,
	!,
    bestPlay(L),
    retract(maxCaptureds(Cant)),
    assert(maxCaptureds(CantCaptureds)),
    retract(bestPlay(L)),
    assert(bestPlay(List)).

flickP2(Grid, X, Y, Color, FGrid):-
	nth0(X,Grid,Column),
	nth0(Y,Column,_Elem),
	setPos(Y,Color,Column,FColumn),
	setPos(X,FColumn,Grid,FGrid).

adyCStar(Origin, Grid, Res) :-
    adyCStarSpread([Origin], [], Grid, Res).

adyCStarSpread([], Vis, _Grid, Vis).

adyCStarSpread(Pend, Vis, Grid, Res):-
    Pend = [P|Ps],
    findall(A,
	        (
	        adyC(P, Grid, A),
		    not(member(A, Pend)),
	    	not(member(A, Vis))
	        ),
            AdyCP),
    append(AdyCP, Ps, NPend),
    adyCStarSpread(NPend, [P|Vis], Grid, Res).

/*
 * adyC(+P, +Grid, -A)
 */

adyC(P, Grid, A):-
    ady(P, Grid, A),
    color(P, Grid, C),
    color(A, Grid, C).

/*
 * ady(+P, +Grid, -A)
 */

ady([X, Y], Grid, [X1, Y]):-
    length(Grid, L),
    X < L - 1,
    X1 is X + 1.

ady([X, Y], _Grid, [X1, Y]):-
    X > 0,
    X1 is X - 1.

ady([X, Y], Grid, [X, Y1]):-
    Grid = [F|_],
    length(F, L),
    Y < L - 1,
    Y1 is Y + 1.

ady([X, Y], _Grid, [X, Y1]):-
    Y > 0,
    Y1 is Y - 1.


/*
 * color(P, Grid, C)
 */

color([X,Y], Grid, C):-
    nth0(X, Grid, F),
    nth0(Y, F, C).
