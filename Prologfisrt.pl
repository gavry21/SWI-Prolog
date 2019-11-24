parent(tom,ann).% Отношение.
parent(tom,mike).
parent(mary,mike).
parent(mike,john).
parent(mike,sue).
parent(mike,nick).
parent(john,jane).
parent(john,jim).
parent(sue,bob).
parent(sue,mark).
man(tom).
man(mick).
man(nick).
man(john).
man(jim).
man(sue).
woman(mary).
woman(ann).
woman(jane).
grandchild(X,Y):-parent(X,Z),parent(Y,Z).
haschild(X):-parent(X,_).
sister(X,Y):-woman(X), parent(Z,X), parent(Z,Y),dif(X,Y).
ancestor(X,Y):-parent(X,Y).
%ancestor(X,Y):-parent(Y,Z), ancestor(X,Z).
ancestor(X,Y):-parent(X,Z), ancestor(Z,Y).
patriarch(X):-parent(X,_),not(parent(_,X)).
cousin(X,Y):-parent(Z,X), parent(T,Y), parent(S,T), parent(S,Z),dif(X,Y).
married(X,Y):-parent(Z,X), parent(Z,Y), sister(X,Y).
relate(X,Y):-ancestor(Z,X), ancestor(Z,Y).
list([]):-fail.

%отсечение
fact(1,1):-!.%все переменные после отсечения отбрасываются
fact(X,Y):- X1 is X-1, fact(X1,Y1), Y is Y1*X.
%четырехместный предикат
facti(X,Y):-factn(X,Y,1,1).
%factn(X,Y,I,P):- I=<X,!, P1 is P*I, I1 is I+1, factn(X,Y,I1,P1).
%factn(_,P,_,P). %эта строка отвечает за 'иначе', если I>X

factn(X,P,X,P):-!.
%если это не так:
factn(X,Y,I,P):- I1 is I+1, P1 is P*I1, factn(X,Y,I1,P1).
%Фибоначчи
fib(1,1):-!.
fib(2,1):-!.
fib(X,Y):-X1 is X-1, fib(X1,Y1), X2 is X-2, fib(X2,Y2), Y is Y1+Y2.

fibi(X,Y):-fibn(X,Y,1,0,1).
fibn(X,P,X,_,P):-!.
fibn(X,Y,I,T,P):- P1 is T+P, I1 is I+1, fibn(X,Y,I1,P,P1).

nod(A,0,C):- C is A,!.
nod(0,A,C):- C is A,!.
nod(A,B,C):- A>=B,!, X is A-B, nod(B,X,C).
nod(A,B,C):- X is B-A, nod(A,X,C).

%НОК
nod(X,Y,Y):-X mod Y=0,!.
nod(X,Y,Z):-Ost=X,nod(Y,Ost,Z).
nok(X,Y,Z):-nod(X,Y,Z1),Z=(X*Y) div Z1.

%минимум
min(X,Y,X):-X=<Y.
min(X,Y,Y):-X>Y.

%максимум
max(X,Y,Z):-X>Y,Z is X,!.
max(_,Y,Z):-Z is Y,!.


%сумма
sum([],0):-!.
sum([X|Y],Z):-sum(Y,Z1), Z is Z1+X.

%i элемент списка
find(0,[X|Y],X):-!.
find(I,[X|Y],Z):-I1 is (I-1), find(I1,Y,Z).

%проверить является ли элементом списка
member(X,[X|_]):-.
member(X,[_|Y]):-member(X,Y).

%минимум
minal(X,[X]):-!.
minal(X,[Y|Z]):-minal(X1,Z), Y=<X1,!, X is Y.
minal(X,[Y|Z]):-minal(X,Z).

%перевернуть список
reverse(X,Y):-reverse(X,[],Y).
reverse([],Y,Y):-!.
reverse([X|Y],Z,T):-reverse(Y,[X|Z],T).

%список является частью другого списка
%является началом списка
isbegin([],_):-!.
isbegin([X|Y],[X|Z]):-isbegin(Y,Z).
%является любой частью списка
sublist(X,Y):-isbegin(X,Y),!.
sublist(X,[Y|Z]):-sublist(X,Z).

%удалить из списка
delete([],_,[]):-!.
delete([X|Y],X,Z):-delete(Y,X,Z),!.
delete([X|Y],T,[X|Z]):-delete(Y,T,Z).

%склеить 2 списка
/*add([],X,X):-!.
add([X1|Y1],Z,[X1|Y2]):-add(Y1,Z,Y2).
*/


point(1,1).
point(2,3).
segment(point(2,3),point(1,1)).
triangle(point(2,3),point(1,1),point(5,2)).
vertical(segment(point(X,_),point(X,_))).
horizontal(segment(point(_,Y),point(_,Y))).


list([]):-fail.
list([_|_]).

member_c(E,[X|_]):-!.
member_c(E,[_|R]):- member_c(E,R).

% длина списка
lengh([],0):-!.
lengh([X|Y],N):-length(Y,M), N is M+1.

%минимальный элемент списка
min_list([X],X):-!.
min_list([X|Y],Z):-min_list(Y,M), M<X,!, Z=M; Z=X.

%есть ли повторяющиеся
same([]):-!.
same([X|Y]):- member(X,Y),!,fail;same(Y).

%сортировка пузырьком
swa([X,Y|Z],[Y,X|Z]):-X>Y,!.
swa([X|Y],[X|Z]):-swa(Y,Z).
bubble([],[]):-!.
bubble(X,Y):- swa(X,Z),!,bubble(Z,Y).
bubble(X,X).

%быстрая сортирока
div(_,[],[],[]).
div(X,[Y|Z],[Y|S],L):- X>Y,!,div(X,Z,S,L).
div(X,[Y|Z],S,[Y|L]):-div(X,Z,S,L).
link([],L,L).
link([X|L1],L2,[X|L3]):-link(L1,L2,L3).
fast([],[]).
fast([X|Y],Z):-div(X,Y,S,L),fast(S,N),fast(L,M), link(N,[X|M],Z).

%является i элементом
ielem(1,X,[X|Y]):-!.
ielem(I,Z,[X|Y]):- I1 is I-1, ielem(I1,Z,Y).


%х находится слева от у
%isleft(X,Y,[X,Y|_]):-!.
%isleft(X,Y,[Z|T]):- X\=Z, !, isleft(X,Y,T).
%isleft(X,Y,[Z,S|T]):- Y\=S, isleft(X,Y,[S|T]).
isleft(X,Y,T):-append(_,[X,Y|_],T).


near(X,Y,T):-append(_,[X,Y|_],T).
near(Y,X,Y):-append(_,[Y,X|_],T).

%национальность
%напиток
%сигареты
%животное
%цвет
%найти хозяина рыбки


enstein(Houses):- Houses=[_,_,_,_,_],
    ielem(1,[nort,_,_,_,_],Houses),
    ielem(3,[_,milk,_,_,_],Houses),
    isleft([_,_,_,_,green],[_,_,_,_,white],Houses),
    near([_,_,malb,_,_],[_,_,_,cat,_],Houses),
    near([_,_,_,horse,_],[_,_,danch,_,_],Houses),
    near([nort,_,_,_,_],[_,_,_,_,blue],Houses),
    near([_,_,malb,_,_],[_,water,_,_,_],Houses),
    member([eng,_,_,_,red],Houses),
    member([swe,_,_,dog,_],Houses),
    member([dat,tea,_,_,_],Houses),
    member([_,coffee,_,_,green],Houses),
    member([_,_,pallmall,bird,_],Houses),
    member([_,_,danch,_,yellow],Houses),
    member([_,bear,winf,_,_],Houses),
    member([germ,_,rotmans,_,_],Houses).


%string

pr3(X):- string_concat(X,",", Z), write(Z),write(Z),write(Z),string_length(X,L), write(L).
%первый, серединный, последний элемент строки
pr4(X):- string_length(X,L), Y is L mod 2, Y =1,!, D is L div 2, sub_string(X,D,1,_,Z1),
sub_string(X,0,1,_,S),write(S),write(" "), write(Z1),write(" "), sub_string(X,_,1,0,Y1),write(Y1).

pr4(X):- string_length(X,L), Y is L mod 2, Y =0,!, D is L div 2,
sub_string(X,0,1,_,S),write(S),write(" "), sub_string(X,_,1,0,Y1),write(Y1).
%вывести номера элементов, совпадающие с последним
pr5(X):- sub_string(X,_,1,0,S), write(S), write(" "), sub_string(X,Y,_,_,S), Z is Y + 1, write(Z).
%удалить все вхождения abc
pr6(X,Y):- split_string(X,"abc","abc",S), atomics_to_string(S,Y).
%удалить лишние пробелы
pr7(X,Y):- split_string(X,"   ","   ",S), atomics_to_string(S,Y).


%первая индивидуальная
%самый встречаемый
%вызов и ответ
task(X,Y):- maxRepeated(X,Z),indices(X,Z,Y),!.
maxRepeated([], []).
maxRepeated(L, E) :- msort(L, [H|T]),
    maxRepeated(T, H, H, 1, 0, E).
%после удаление последнего элемента - проверка на количество вхождений
maxRepeated([], H, _, C1, C2, H) :- C1 >= C2.
maxRepeated([], _, X, C1, C2, X) :- C1 < C2.

maxRepeated([H|T], H, LastF, C1, C2, E) :- maxRepeated(T, H, LastF, C1 + 1, C2, E).

maxRepeated([X|T], H, LastF, C1, C2, E) :-
    (C1 > C2 ->  maxRepeated(T, X, H, 1, C1, E);
    maxRepeated(T, X, LastF, 1, C2, E)).

%индекс
indices(List, E, Is) :-findall(N, nth1(N, List, E), Is).

%вторая индивидуальная
andrew(R,A):-
    (  (R=vity;R=koly) ->  A=1;  A=0).

vity(R,V):-
    (  (R\=vity,R\=yura) ->  V=1;  V=0).

dima(D):-
    (   (andrew(_,A),vity(_,V),A+V=:=1) ->  D=1;  D=0).

yura(Y):-
    (   dima(0) ->  Y=1;  Y=0).

solve(R):- member(R,[andew,vity,dima,yura,koly]),
    andrew(R,A),vity(R,V),dima(D),yura(Y),A+V+D+Y>=3.

%файл

run:- see('C:/Users/Sofia/Downloads/first.txt'), readln(X), write(X).
fileToList(F,L):- seen, see(F), readfile([],[],L), seen.
readfile(T,R,L):- get0(X), (X<0, (T=[], L=R; name(T1,T),
                  append(R,[T1],L)),!;
                  ( X=32; X=10; X=13 ),
                  (   T=[], readfile(T,R,L), name(T1,T),
                      append(R,[T1],R1), readfile([],R1,L)),!;
                                  X>0, append(T,[X],T1), readfile(T1,R,L),!).


fileToList1(F,L):- seen, see(F), readfile([],[],L), seen,
    atomics_to_string(L," ",R), writeln(R),
    split_string(R,".","", L1), writeln(L1).

run1(L):- fileToList1('C:/Users/Sofia/Downloads/first.txt',L).


%база знаний
%факториал
:- dynamic fact1/2.

fact1(1,1):-!.
fact1(X,Y):- X1 is X-1, fact1(X1,Y1), asserta(fact1(X1,Y1)), Y is Y1*X, !.
%фибоначчи
:- dynamic fibo/2.

fibo(X,Y):-fibon(X,Y,1,0,1).
fibon(X,P,X,_,P):-!.
fibon(X,Y,I,T,P):- P1 is T+P, I1 is I+1, fibon(X,Y,I1,P,P1).
%база студентов
student("Иванов", "25.04.1998", "45679978456").
student("Петров", "05.10.1998", "45679978456").
student("Сидоров", "23.01.1998", "45679978456").
student("Козловский", "12.09.1998", "45679978456").
student("Максимов", "06.11.1998", "45679978456").
student("Удзумаки", "15.03.1998", "45679978456").

:- dynamic studentd/3.
myassert:- student(X,Y,Z), assertz(studentd(X,Y,Z)), fail.
myassert:- !.

%разработать базу данных


pal([], [], 0):-!.
pal([X], [X], 0).
pal(In, Out, N) :-
    % first we check if the first and last letter are the same
       append([H|T], [H], In)
        % we must check that the middle is a palindrome
    ->  pal(T, T1, N1),N is N1+1,
        append([H|T1], [H], Out)
    ;   % if not, we remove the first letter
        % and we work with the rest
        In = [H|T],
        % we compute the palindrome from T
        pal(T,T1,N1),N is N1+1,
        % and we complete the palindrome to
        % fit the first letter of the input
        append([H|T1], [H], Out).

output1(X,Y):-pal(X,_,Y).



%
build_pal([H|T], Out):-
    pal1(T,T1,_),
    append([H|T1], [H], Out).


pal1([], [],0).
pal1([X], [X],0).
pal1(In, Out,N) :-
    (   append([H|T], [H], In)
    ->  pal1(T, T1, N1),N is N1+1,
        (   T = T1
        ->  append([H|T1], [H], Out)
        ;   build_pal(In, Out))
    ;   build_pal(In, Out)).



%деревья
create_tree(X, tree(X,empty,empty)).
insert_left(X,tree(A,_,B),tree(A,X,B)).
insert_right(X,tree(A,B,_),tree(A,B,X)).

tr:-create_tree(5,X),create_tree(6,Y), insert_left(Y,X,Z), write(Z).

tree_member(X,tree(X,_,_)):-!.
tree_member(X,tree(_,L,_)):-tree_member(X,L),!.
tree_member(X,tree(_,_,R)):-tree_member(X,R).
%глубина
tree_height(empty,0):-!.
tree_height(tree(_,L,R),D):-tree_height(L,D1),tree_height(R,D2),max(D1,D2,M),D is M+1.

tr1:-create_tree(5,X),create_tree(6,Y), insert_left(Y,X,Z),tree_height(Z,A), write(A).

%количество элементов дерева
tree_elem(empty,0):-!.
tree_elem(tree(_,L,R),D):-tree_elem(L,D1), tree_elem(R,D2), D is D1+D2+1.

tr_elem:-create_tree(5,X),create_tree(6,Y), insert_left(Y,X,Z),tree_elem(Z,A), write(A).

%сортировка
treesort(X,Y):-sort_tree(X,Tree),tree_list(Tree,Y).
sort_tree([],empty):-!.
sort_tree([X|Y],Tree):-sort_tree(Y,Tree1),ins(X,Tree1,Tree).
ins(X,empty,tree(X,empty,empty)):-!.
ins(X,tree(Y,L,R),tree(Y,L,R1)):-X>=Y,ins(X,R,R1),!.
ins(X,tree(Y,L,R),tree(Y,L1,R)):-ins(X,L,L1).
tree_list(empty,[]).
tree_list(tree(X,L,R),List):-tree_list(L,D1), tree_list(R,D2), append(D1,[X|D2],List).
%
%
%
%%%%%%%%%%%%%
p(1,2,1).
p(1,3,2).
p(2,4,1).
p(3,4,2).
p(4,5,3).

find_all(X,G,Bag):- post_it(X,G), gather([],Bag).
post_it(X,G):- call(G), asserta(data999(X)),fail.
post_it(_,_).
gather(B,Bag):- data999(X), retract(data999(X)),gather([X|B],Bag),!.
gather(B,B).

search(X,Y,[X,Y]):- p(X,Y,_);p(Y,X,_),!.
search(X,Y,[X|T]):- (p(X,Z,_);p(Z,X,_)),search(Z,Y,T),
    not(member(X,T)),!.
search(X,Y,T):- (p(X,Z,_);p(Z,X,_)),search(Z,Y,T).

%четвертая идивидуальная

means(0,zero).
means(1,one).
means(2,two).
means(3,three).
means(4,four).
means(5,five).
means(6,six).
means(7,seven).
means(8,eight).
means(9,nine).
means(10,ten).

openf:- exists_file('C:/Users/Sofia/Downloads/IN.txt'), !, see('C:/Users/Sofia/Downloads/IN.txt'), read(f1), see('C:/Users/Sofia/Downloads/test.txt'), write(f2), seen.

translate:-!.
translate:- not(eof(f1)), get_char(C), Y=C, means1(Y), transform.
means1(Y):- means(Y,A), write(A).
means1(Y):-write(Y).

tryth:-openf,translate.
