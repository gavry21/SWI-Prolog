% check of uniquness of list elements
unique([]):-!.
unique([X|L]):-not(member(X,L)),unique(L),!.
% finds element on I place in the list
iEl1([X|_],0,X):-!.
iEl1([_|Y],I,Z):-I1 is I-1,iEl1(Y,I1,Z),!.
% finds first I of element in the list
iEl2([X|_],0,X):-!.
iEl2([_|Y],I,Z):-iEl2(Y,I1,Z),I is I1+1,!.

% checks if list is ordered by strong ascend
asc([]):-!.
asc([_]):-!.
asc([X,Y|Rest]):-X<Y,asc([Y|Rest]),!.

% true если List_Out это List_In без элемента списка List_Rem
rem_list([],[],_,_):-!.
rem_list([X|List1],[X|List2],List_Rem,N):-
    N1 is N+1,
    not(member(N,List_Rem)),
    rem_list(List1,List2,List_Rem,N1),!.
rem_list([_|List1],List2,List_Rem,N):-N1 is N+1,
    member(N,List_Rem),
    rem_list(List1,List2,List_Rem,N1),!.
rem_list(List_In,List_Out,List_Rem):-
    rem_list(List_In,List_Out,List_Rem,1),!.

% true если оба списка равны без одного и того же i-го элемента
equal_except_i([],[],_):-!.
equal_except_i([X|L1],[X|L2],I):-I1 is I-1,equal_except_i(L1,L2,I1),!.
equal_except_i([_|L1],[_|L2],0):-equal_except_i(L1,L2,-1),!.

% возвращаем номера элементов списка в виде списка N,N-1,..,1
start_list_reversed([],0):-!.
start_list_reversed([N|List],N):-N1 is N - 1,
    start_list_reversed(List,N1),!.
% reverses list from previous statement
start_list(List,N):-
    start_list_reversed(List_Reversed,N),
    reversed(List,List_Reversed).

end_list([],0,_):-!.
end_list([K|List],M,N):-K is N-M,M1 is M-1,end_list(List,M1,N),!.


% проверка на палином
reversed(X,Y):-reversed(X,[],Y).
reversed([],Y,Y):-!.
reversed([X|Y],Z,T):-reversed(Y,[X|Z],T).

polyn_list(List):-
    reversed(List,List_R),
    List=List_R,!.
% checks if List with romved elements from rem is polyndrome
polyn_check(List,Rem):-
    rem_list(List,Res,Rem),
    polyn_list(Res),!.

pc_and_asc(Word,Rem,1):-
    asc(Rem),
    polyn_check(Word,Rem),
    write(Rem),
    rem_list(Word,Test,Rem),
    write(Test),
    !.
pc_and_asc(Word,Rem,0):-
    not((asc(Rem),
    polyn_check(Word,Rem))),!.

isbegin([],_):-!.
isbegin([X|Y],[X|Z]):-isbegin(Y,Z).
issubstring(X,Y):-isbegin(X,Y),!.
issubstring(X,[_|Z]):-issubstring(X,Z).

len([],0):-!.
len([_|Z],N):-len(Z,N1),N is N1+1,!.


% итерации по Pos елементу списка
iter(Word,List,Pos,N,Pol_Am):-
    ((len(List,K),
      N1 is N+1,
     T is K-Pos,
     end_list(EL,T,N1),
     issubstring(EL,List));
    iEl1(List,Pos,N)),
    pc_and_asc(Word,List,Pol_Am),!.
iter(Word,List,Pos,N,K):-iEl1(List,Pos,X),
    len(List,Length),
    Pos1 is Pos+1,(
    (Pos1<Length,iter(Word,List,Pos1,N,H));
    (Pos1>=Length,pc_and_asc(Word,List,H))),
    X1 is X+1,
    iEl1(List_Next,Pos,X1),
    equal_except_i(List,List_Next,Pos),
    iter(Word,List_Next,Pos,N,G),K is G+H,!.

wrap_iter(Word,PA,0):-pc_and_asc(Word,[],PA),!.
wrap_iter(Word,PA,N):-
    start_list(List,N),
    len(Word,K),
    iter(Word,List,0,K,A),
    N1 is N-1,
    wrap_iter(Word,PP,N1),
    PA is PP+A,!.

task3(String,Anwser):-
    string_chars(String,Word),
    len(Word,N),
    wrap_iter(Word,Anwser,N),!.





