% ..................THREESUM USING DEPTH FIRST SEARCH (UNINFORMED).......

move([State,Sum],[NS,Nsum],I):-
    len(Sum,R1),
    R1<3,
    len(State,R2),
    I<R2,
    nth0(I,State,E),
    substitute(E,State,#,NS),
    insertAtListEnd(E,Sum,Nsum),!.


getallchilderen([State,Sum], Open ,Closed , Childeren):-
    last(State,#,Z),
    X is Z+1,
    getallchilderen([State,Sum],Open,Closed ,[],Childeren,X).

getallchilderen(State,Open,Closed,Arr,Childeren,Index):-
    movees(State,Next,Index,Open,Closed),
    insertAtListEnd(Next,Arr,NewArr),
    NWindex is Index+1,!,
    getallchilderen(State,Open,Closed,NewArr,Childeren,NWindex).
getallchilderen(_,_,_,Childeren,Childeren,_).


movees([State,Sum],[R,S],I,Open,Closed):-
            move([State,Sum],[R,S],I),
		\+ member([R,_],Open),
		\+ member([R,_],Closed).

threeSum(Start,Goal,Output):-
		algo([ [Start,[]],[] ],[],Goal,Output).

algo([ [State,[A,B,C] ]|_],Closed,Goal,Output):-
    S is A+B+C,
      S =:= Goal,
    Output = [A,B,C].

algo(Open, Closed, Goal,Output):-
		removeFromOpen(Open, Statee, RestOfOpen),
		getallchilderen(Statee, Open, Closed, Children),
		addListToOpen(Children , RestOfOpen, NewOpen),
		algo(NewOpen,[ Statee|Closed], Goal,Output).



removeFromOpen([[State,S]|RestOpen], [State,S], RestOpen).


addListToOpen( [],Children, Children).
addListToOpen([H|Open],Children,[H|NewOpen]):-
		addListToOpen( Open,Children, NewOpen).


substitute(_, [], _, []):-!.
substitute(Element1, [Element1|T], Element2, [Element2|T1]):-
	substitute(Element1, T, Element2, T1),!.

substitute(Element1, [H|T], Element2, [H|T1]):-
	substitute(Element1, T,Element2 , T1).


len([],0).
len([H|T],R):-
len(T,R1),
R is 1+R1.


insertAtListEnd(Element,[ ],[Element]).
insertAtListEnd(Element,[H|T],[H|NewT]) :- insertAtListEnd(Element,T,NewT).

last(List, Element, Result) :- last1(List, Element, 0, -1, Result).

last1([H| T], Element, Index, Acc, Result) :- Index2 is Index + 1,
    (H == Element, !, last1(T, Element, Index2, Index, Result);
    last1(T, Element, Index2, Acc, Result)).
last1([], _, _, Acc, Acc).
