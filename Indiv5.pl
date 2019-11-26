%����� �������������
run:- consult('C:/Users/Sofia/Downloads/mydata.pl'),
    retractall(village/2),
    menu.

menu:-repeat,
    write('���� ������'),nl,nl,
    write('1 - ������ � ��������'),nl,
    write('2 - ������ � ����������'),nl,
    write('3 - ������ � ��������'),nl,
    read(X),
    X<4,
    check(X).

check(1):-menu2.
%check(2):-menu3.
%check(3):-menu4.



menu2:-repeat,
    write('���� ������ � ������-��������'),nl,nl,
    write('1 - ���������� ������ � ��������'),nl,
    write('2 - �������� ������ � �������'),nl,
    write('3 - ������� ������ �� �������'),nl,
    write('4 - ��������� ���� � ����'),nl,
    read(X),
    X<5,
    process(X).


process(1):-viewdata.
process(2):-addvillage.
process(3):-deletevillage.
process(4):-dbsave.


adddata:- village(X,Y), asserta(village(X,Y)), fail.
adddata:-!.


viewdata:- village(X,Y), write("Name: "), write(X),nl, repeat.

addvillage:-write("add village"),nl,nl,
    repeat, write("name: "),
    read(X),
    write("id: "), read(Y),
    assertz(village(X,Y)).

deletevillage:- write('delete village'),nl,nl,
    write('write village name: '),
    read(X),
    retract(village(X,_)),
    write('village deleted'),nl,nl,
    tell('C:/Users/Sofia/Downloads/mydata.pl'), listing(village),
    told, write('db is saved').

dbsave:- tell('C:/Users/Sofia/Downloads/mydata.pl'), listing(village),
    told, write('db is saved').
