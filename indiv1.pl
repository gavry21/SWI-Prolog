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


