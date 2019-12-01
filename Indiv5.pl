%����� �������������
run:- consult('C:/Users/Sofia/Downloads/mydata.pl'),
    retractall(village/2),retractall(tech/2), retractall(character/3),
    menu.

menu:-repeat,
    write('���� ������'),nl,nl,
    write('1 - ������ � ��������'),nl,
    write('2 - ������ � ��������'),nl,
    write('3 - ������ � ����������'),nl,
    read(X),
    X<4,
    check(X).

check(1):-menu2.
check(2):-menu3.
check(3):-menu4.

menu2:-repeat,
    write('���� ������ � ������-��������'),nl,nl,
    write('1 - ���������� ������ � ��������'),nl,
    write('2 - �������� ������ � �������'),nl,
    write('3 - ������� ������ �� �������'),nl,
    write('4 - ��������� ���� � ����'),nl,
    read(X),
    X<5,
    process(X).

menu3:-repeat,
    write('���� ������ � ��������'),nl,nl,
    write('1 - ���������� ������ � ��������'),nl,
    write('2 - �������� ������ � �������'),nl,
    write('3 - ������� ������ �� �������'),nl,
    write('4 - ��������� ���� � ����'),nl,
    read(X),
    X<5,
    process1(X).

menu4:-repeat,
    write('���� ������ ����������'),nl,nl,
    write('1 - ���������� ������ � ����������'),nl,
    write('2 - �������� ������ � �������'),nl,
    write('3 - ������� ������ �� �������'),nl,
    write('4 - ��������� ���� � ����'),nl,
    read(X),
    X<5,
    process2(X).

process(1):-viewdata.
process(2):-addvillage.
process(3):-deletevillage.
process(4):-dbsave.

process1(1):-viewdata1.
process1(2):-addtech.
process1(3):-deletetech.
process1(4):-dbsave.

process2(1):-viewdata2.
process2(2):-addcharacter.
process2(3):-deletecharacter.
process2(4):-dbsave.



viewdata1:- tech(_,X),
    write("��������: "), write(X),nl,
    write('------------------------------------'),nl,fail.

viewdata2:-tech(T,Y),village(Z,D),character(X,T,D),
    write("���: "), write(X),nl,
    write("�������: "), write(Y),nl,
    write("�������: "), write(Z),nl,
    write('------------------------------------'),nl,fail.


%adddata:- village(X,Y), asserta(village(X,Y)).
%adddata:-!.

%addcharacter:- character(X,Y,Z), asserta(character(X,Y,Z)).
%addcharacter:-!.

%addtech:- tech(X,Y), asserta(tech(X,Y)).
%addtech:-!.


viewdata:- village(X,_),
    write("��������: "), write(X),nl,
    write('------------------------------------'),nl,fail.

addvillage:-write("�������� �������"),nl,nl,
    repeat, write("��������: "),
    read(X),
    write("id: "), read(Y),
    assertz(village(X,Y)).

addtech:-write("�������� �������"),nl,nl,
    repeat, write("��������: "),
    read(X),
    write("id: "), read(Y),
    assertz(village(X,Y)).

addcharacter:-
    write("�������� ���������"),nl,nl,
    write("���: "),read(X),
    write("�������� �������: "), read(Y),not(village(Y,D)),
    write("����� ������� ���");
    write("�������� �������: "), read(Z),not(tech(T,Z)),
    write("����� ������� ���");
    assertz(character(X,T,D)).

deletevillage:- write('������� �������'),nl,nl,
    write('�������� �������� �������: '),
    read(X),
    retract(village(X,Y)),
    retract(character(_,_,Y)),
    write('village deleted'),nl,nl,
    tell('C:/Users/Sofia/Downloads/mydata.pl'), listing(village),
    listing(character),listing(tech),
    told, write('������� ���������'),nl.

deletetech:- write('������� �������'),nl,nl,
    write('�������� �������� �������: '),
    read(X),
    retract(tech(Y,X)),
    retract(character(_,Y,_)),
    write('tech deleted'),nl,nl,
    tell('C:/Users/Sofia/Downloads/mydata.pl'), listing(village),
    listing(character),listing(tech),
    told, write('������� ���������'),nl.

deletecharacter:- write('������� �������'),nl,nl,
    write('�������� ��� ���������: '),
    read(X),
    retract(character(X,_,_)),
    write('character deleted'),nl,nl,
    tell('C:/Users/Sofia/Downloads/mydata.pl'), listing(village),
    listing(character),listing(tech),
    told, write('������� ���������'),nl.


dbsave:- tell('C:/Users/Sofia/Downloads/mydata.pl'), listing(village),listing(tech),listing(character),
    told, write('���� ���������'),nl,fail.
