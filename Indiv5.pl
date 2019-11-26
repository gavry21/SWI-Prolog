%пятая идивидуальная
run:- consult('C:/Users/Sofia/Downloads/mydata.pl'),
    retractall(village/2),
    menu.

menu:-repeat,
    write('База данных'),nl,nl,
    write('1 - Данные о деревнях'),nl,
    write('2 - Данные о персонажах'),nl,
    write('3 - Данные о техниках'),nl,
    read(X),
    X<4,
    check(X).

check(1):-menu2.
%check(2):-menu3.
%check(3):-menu4.



menu2:-repeat,
    write('База данных о шиноби-деревнях'),nl,nl,
    write('1 - Посмотреть данные о деревнях'),nl,
    write('2 - Добавить данные в таблицу'),nl,
    write('3 - Удалить данные из таблицы'),nl,
    write('4 - Сохранить базу в файл'),nl,
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
