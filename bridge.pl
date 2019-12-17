child(1,[2,3]).
child(2,[1,3]).
child(3,[1,2,4]).
child(4,[3,5]).
child(5,[4,6]).
child(6,[8,7]).
child(7,[6,8]).
child(8,[6,7]).

findbr(R):-findall(A/B,(child(A,L),
                       member(B,L)),X), clear(X,Y),
                   candel(Y,R).
candel([],[]):-!.
candel([A/B|T],P):-candel(T,P1),!,(mydel(A/B),
                                  P=[A/B|P1];P=P1).
mydel(A/B):-
    findall(L,child(L,_),[H|T]),
    findall(G,(member(G,T),pathr(A/B,H,G,[H],_)),M),
    setof(Z,member(Z,M),M1),
    length(T,S1),
    length(M1,S2),
    S1\=S2.
pathr(_,X,X,P,P).
pathr(X/Y,A,B,T,R):-child(A,L),
    member(Z,L),
    not(member(Z,T)),
    A/Z\=X/Y,A/Z\=Y/X,
    pathr(X/Y,Z,B,[Z|T],R).
clear([],[]):-!.
clear([X/Y|T],R):-clear(T,R1),
    (   (member(X/Y,R1); member(Y/X,R1)),
        R=R1;
    R=[X/Y|R1]),!.

