%test:-threeSum([3,8,9,10,12,14],27,Output).
%test:-threeSum([2,4,8,10,12,14],25,Output).
%test:-threeSum([2,4,8,12,14,16],20,Output).
%test:-threeSum([1,2,3,4,5,6,7,8,9],12,Output).
%test:-threeSum([2,5,6,9,10,13,15],30,Output).
%problem specific part
length_List([],0).
length_List([_|T],L):-
    length_List(T,L2),
    L is L2 + 1.
	
sum_List([],0).
sum_List([K|T],L):-
    sum_List(T,L2),
    L is K + L2.

	
checkitem(R,[R|_]):-!.
checkitem(R,[_|T]):-
    checkitem(R,T),!.

member_list(X,[X|_]).
member_list(X,[_|TAIL]) :- member_list(X,TAIL).

append_item(A,T,T) :- member_list(A,T),!.
append_item(A,T,[A|T]).



getHeuristic(State,H,Goal):-
	length_List(State,Len),
	sum_List(State,Sum),
	(
	   Len>3 -> H is 5000;
	   Len==3,Sum<Goal ->H is 2000;
	   Sum >Goal -> H is 1000;
	   Sum == Goal -> H is 0 ;
	   H is Goal-Sum
	).
	
	
insert_sort(List,Sorted):-i_sort(List,[],Sorted),!.
i_sort([],Acc,Acc).
i_sort([H|T],Acc,Sorted):-insert(H,Acc,NAcc),i_sort(T,NAcc,Sorted).
   
insert(X,[Y|T],[Y|NT]):-X>Y,insert(X,T,NT).
insert(X,[Y|T],[X,Y|T]):-X=<Y.
insert(X,[],[X]).
	

%general algorithm

threeSum(Start,Goal,Output):-
	path([[[],Start,5000]],[],Goal,Output).
	
length_ok(3).
goal_ok(Num1,Num1).

path([], _, _,_):-fail,!.%if open list is empty and i dont find any solution
path(Open, _,Goal,Output):-
	getBestChild(Open, [Output, _, H], _),
	length_List(Output,Length),
	length_ok(Length),
	sum_List(Output,Sum),
	H==0,
	goal_ok(Sum,Goal).	
	
path(Open, Closed, Goal,Output):-
	getBestChild(Open, [State, Parent, H], RestOfOpen),!,
	getallchilderen([State, Parent, H], Open, Closed, Children, Goal),
	addListToOpen(Children , RestOfOpen, NewOpen),
	path(NewOpen, [[State, Parent, H] | Closed], Goal,Output).
	
getallchilderen([State, Parent, H], Open ,Closed , Childeren, Goal):-
    getallchilderen([State,Parent,H],Open ,Closed ,[],Childeren,Goal).

getallchilderen([List,[F|T],H],Open ,Closed ,Arr,Childeren,Goal):-
    moves(List,[F|T],Next,Open, Closed, Goal), 
	not(checkitem(Next,Arr)),!,
    append_item(Next,Arr,NewArr),!,
    getallchilderen([List,T,H],Open,Closed,NewArr,Childeren,Goal),!.
getallchilderen(_,_,_,Childeren,Childeren,_). 
  
moves(List,[F|T],[Next,T, H],Open, Closed, Goal):-
   append_item(F,List,NwNext),!,
   insert_sort(NwNext,Next),
   \+ member([Next,_,_],Open),
   \+ member([Next,_,_],Closed),
   getHeuristic(Next, H, Goal).

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
	H < H1, !.
getBest([_, _, _], [State1, Parent1, H1], [State1, Parent1,H1]).


removeFromList(_, [], []):-!.
removeFromList(H, [H|T], V):-
	!, removeFromList(H, T, V).
removeFromList(H, [H1|T], [H1|T1]):-
	removeFromList(H, T, T1).