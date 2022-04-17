:- module(proylcc, 
	[  
		flickSheall/4,
		searchAdj/4
	]).

:-use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% flick(+Grid, +FilaIndex, +ColumnaIndex, +Color, -FGrid)
%
% FGrid es el resultado de hacer 'flick' de la grilla Grid con el color Color.
% Retorna false si Color coincide con el color de la celda (FilaIndex, ColumnaIndex)
% de la grilla. 

flickSheall(Grid, Adjs, Color, FGrid):-
	Adjs = [F|Fs],
	F = [FilaIndex|[ColumnaIndex]],
	flickSheall(Grid, Fs, Color, FFGrid),
	flick(FFGrid, FilaIndex, ColumnaIndex, Color, FGrid).
flickSheall(Grid, [], _Color, Grid).

flick(Grid, FilaIndex, ColumnaIndex, Color, FGrid):-
	move(Grid, FilaIndex, ColumnaIndex, Color, FGrid),
	Grid \= FGrid.
flick(Grid, [], [], _Color, Grid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% move(+Grid, +FilaIndex, +ColumnaIndex, +Color, -FGrid)
%
% move busca la fila numero FilaIndex en Grid y llama al procedimiento replace.
% retorna en FGrid la grilla con el 'flick' realizado.

move([H|T], FilaIndex, ColumnaIndex, Color, [H|R]):-
	FilaIndex > -1, FI is FilaIndex-1, move(T, FI, ColumnaIndex, Color, R).
move([F|Fs], 0, ColumnaIndex, Color, [NewL|Fs]):- 
	replace(F, ColumnaIndex, Color, NewL), NewL \= false.
move(L, _, _, _, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% replace(+List,+Index,+Value,-NewList).
%
% Busca y remplaza el elemento(ValueOld) ubicado en la posicion Index por el valor 
% Value y lo retorna en NewList.

replace([H|T], Index, Value, [H|R]):-
	Index > -1, NI is Index-1, replace(T, NI, Value, R).
replace([ValueOld|T], 0, Value, [Value|T]):- ValueOld \= Value.
replace(L, _, _, L).

/* Algoritmo 3: Funcional pero muy costoso TODO: Optimizar */
/**/
searchAdj(Grid, Adjs, Color, ListAdj):-
	adj(Grid, Adjs, Color, [[]|ListAdj]).
	/*remove_dups(ListAux,ListAdj).*/


remove_dups([], []).
remove_dups([Primera | Rest], Newrest):-
   member(Primera, Rest),
   remove_dups(Rest, Newrest).

remove_dups([Primera | Rest], [Primera | Newrest]):-
   not(member(Primera, Rest)),
   remove_dups(Rest, Newrest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
adj(Grid, Adjs, Color, [[]|ListAdj]):-
	Adjs = [F|Fs],
	F = [X,Y],
	adj(Grid, Fs, Color, NewList),
	adjacentes(Grid, [NewList], X, Y, Color, ListAdj).

adj(_Grid, [], _Color, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
adjacentes(Grid, Adjs, X, Y, Color, ListAdj):-
	X1 is X-1,
	buscarAdj(Grid, Adjs, X1, Y, Color, L1),
	X2 is X+1,
	buscarAdj(Grid, Adjs, X2, Y, Color, L2),
	Y1 is Y-1,
	buscarAdj(Grid, Adjs, X, Y1, Color, L3),
	Y2 is Y+1,
	buscarAdj(Grid, Adjs, X, Y2, Color, L4),
	/* si L1==L2==L3==L4 return Adjs*/
	append(L1, L2, L5), append(L3, L4, L6),
	append(L5, L6, ListAux), remove_dups(ListAux,ListAux2),
	union(ListAux2, [[X,Y]], ListAux3),
	delete(ListAux3, [], ListAdj).

buscarAdj(Grid, Adjs, X, Y, Color, List):-
	searchRow(Grid, Adjs, X, Y, Color, Equal),
	Equal \= false,
	not(member([X,Y], Adjs)),
	select([X,Y],NewAdjs,Adjs),
	adjacentes(Grid, NewAdjs, X, Y, Color, List).

buscarAdj(_Grid, _Adjs, _X, _Y, _Color, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
searchRow([_H|T], Adjs, FilaIndex, ColumnaIndex, Color, Equal):-
		FilaIndex > -1, FI is FilaIndex-1, searchRow(T, Adjs, FI, ColumnaIndex, Color, Equal).
searchRow([F|_Fs], Adjs, 0, ColumnaIndex, Color, Equal):-
		searchColum(F, Adjs, ColumnaIndex, Color, Equal).
searchRow(_L, _Adjs, _, _, _, false).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
searchColum([_H|T], Adjs, Index, Color,	Equal):-
	Index > -1, NI is Index-1, searchColum(T, Adjs, NI, Color, Equal).
searchColum([ValueOld|_T], _Adjs, 0, Color, true):- ValueOld == Color.
searchColum([_ValueOld|_T], _Adjs, 0, _Color, false).
searchColum(_L, _Adjs, _, _Color, false).
/* fin alg 3*/