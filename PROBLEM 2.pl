test:-deletiveEditing(['D','E','T','E','R','M','I','N','E','D'], ['T','R','M','E']).
test:-deletiveEditing(['D','E','T','E','R','M','I','N','E','D'], ['T','E','R','M']).
test:- deletiveEditing(['D','E','I','N','S','T','I','T','U','T','I','O','N','A','L','I','Z','A','T','I','O','N'], ['D','O','N','A','T','I','O','N']).
test:-deletiveEditing(['C','O','N','T','E','S','T'], ['C','O','D','E']).
test:- deletiveEditing(['S','O','L','U','T','I','O','N'], ['S','O','L','U','T','I','O','N']).

%problem specific part


check(R,[R|_]):-!.
check(R,[_|T]):-
    check(R,T),!.

memberOfList(X,[X|_]).
memberOfList(X,[_|TAIL]) :- memberOfList(X,TAIL).

append_item(A,T,T) :- memberOfList(A,T),!.
append_item(A,T,[A|T]).

isZero(0).

remove_duplicates([],[]):-!.

remove_duplicates([H | T], List) :-
    member(H, T),!,
    remove_duplicates( T, List).

remove_duplicates([H | T], [H|T1]) :-
    \+member(H, T),!,
    remove_duplicates( T, T1).

delete_one(_, [], []):-!.
delete_one(Term, [Term|Tail], Tail):-!.
delete_one(Term, [Head|Tail], [Head|Result]) :-
  delete_one(Term, Tail, Result),!.

%general algorithm
deletiveEditing(Start,Goal):-
	getHeuristic(Start, H, Goal),
	path([[Start,null,H]],[],Goal),!.%open, closed, goal, path_cost, heuristic, total cost

path([], _, _):-
	not(isokay(_)),!.
path(Open, _, Goal):-
	getBestChild(Open, [Goal, _, _], _).


path(Open, Closed, Goal):-
	getBestChild(Open, [State, Parent, H], RestOfOpen),
	getallchilderen(State, Open, Closed, Children, Goal),
	addListToOpen(Children , RestOfOpen, NewOpen),
	path(NewOpen, [[State, Parent, H] | Closed], Goal).


getallchilderen(State, Open ,Closed , Childeren, Goal):-
    getallchilderen(State,Open ,Closed ,[],Childeren,0, Goal).

getallchilderen(State,Open ,Closed ,Arr,Childeren,Index, Goal):-
    moves(State,Next,Index,Open, Closed, Goal),
	not(check(Next,Arr)),!,
    append_item(Next,Arr,NewArr),!,
    NWindex is Index +1,
    getallchilderen(State,Open,Closed,NewArr,Childeren,NWindex,Goal).
getallchilderen(_,_,_,Childeren,Childeren,_,_).

moves(State,[Next,State, H],Index,Open, Closed, Goal):-
   remove_duplicates(State,Distinct),
   nth0(Index, Distinct, Elem),
   delete_one(Elem,State,Next),
   \+ member([Next,_,_],Open),
   \+ member([Next,_,_],Closed),
   getHeuristic(Next, H, Goal),
   isokay(Next).

isokay(_):-!.

getHeuristic([], 0, []):-!.
getHeuristic([], 1000, _):-!.

getHeuristic([H|T1],V,[H|T2]):-!,
	getHeuristic(T1,V, T2),!.

getHeuristic([_|T1],H,T2):-
	getHeuristic(T1,NWH, T2),
	H is NWH + 1 .


addListToOpen(Children, [], Children).
addListToOpen(Children, [H|Open], [H|NewOpen]):-
	addListToOpen(Children, Open, NewOpen).

getBestChild([Child], Child, []).
getBestChild(Open, Best, RestOpen):-
	getBestChild1(Open, Best),
	removeFromList(Best, Open, RestOpen).

getBestChild1([State], State):-!.
getBestChild1([State|Rest], Best):-
	getBestChild1(Rest, Temp),
	getBest(State, Temp, Best).

getBest([State, Parent, H], [_, _, H1], [State, Parent, H]):-
	H < H1,!.
getBest([_, _, _], [State1, Parent1, H1], [State1, Parent1,H1]).


removeFromList(_, [], []):-!.
removeFromList(H, [H|T], V):-
	!, removeFromList(H, T, V).
removeFromList(H, [H1|T], [H1|T1]):-
	removeFromList(H, T, T1).




















