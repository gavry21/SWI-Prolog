%четвертая идивидуальная

%чтение и получение слов из файла
readfile(T,R,L):- get0(X), (X<0, (T=[], L=R; name(T1,T),
                  append(R,[T1],L)),!;
                  ( X=32; X=10; X=13 ),
                  (   T=[], readfile(T,R,L), name(T1,T),
                      append(R,[T1],R1), readfile([],R1,L)),!;
                                  X>0, append(T,[X],T1), readfile(T1,R,L),!).


fileToList(F,Y):- seen, see(F), readfile([],[],L), seen,
    atomics_to_string(L,"",R), writeln(R), atom_codes(A,R),
    atom_chars(A, Y), writeln(Y).


%запись в файл
insertInFile(X):- seen,
    tell('C:/Users/Sofia/Downloads/test.txt'), writeInFile(X),told,seen.

%предикат записи в файл
writeInFile([]):-!.
writeInFile([A|T]):- write(A), writeInFile(T).

means('0','ноль').
means('1','один').
means('2',два).
means('3',три).
means('4',четыре).
means('5',пять).
means('6',шесть).
means('7',семь).
means('8',восемь).
means('9',девять).
means('10',десять).
means(X,X).

translate([],[]).
translate([X|List],[What|ConvertedList]):- means(X,What),
    translate(List,ConvertedList).

%translate([]):-!.
%translate(X):- get_char(C), Y = C, means1(Y), translate(X).
%means1(Y):- means(Y,A), write(A).
%means1(Y):-write(Y).

solution(Y):- fileToList('C:/Users/Sofia/Downloads/IN.txt',X), translate(X,Y),
    insertInFile(Y).
