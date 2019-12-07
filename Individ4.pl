% digits and number parts in written form
digit_to_word(0,'',_):-!.
digit_to_word(1,'���� ',m):-!.
digit_to_word(1,'���� ',f):-!.
digit_to_word(2,'��� ',m):-!.
digit_to_word(2,'��� ',f):-!.
digit_to_word(3,'��� ',_):-!.
digit_to_word(4,'������ ',_):-!.
digit_to_word(5,'���� ',_):-!.
digit_to_word(6,'����� ',_):-!.
digit_to_word(7,'���� ',_):-!.
digit_to_word(8,'������ ',_):-!.
digit_to_word(9,'������ ',_):-!.

ten_to_word(0,''):-!.
ten_to_word(1,'������ '):-!.
ten_to_word(2,'�������� '):-!.
ten_to_word(3,'�������� '):-!.
ten_to_word(4,'����� '):-!.
ten_to_word(5,'��������� '):-!.
ten_to_word(6,'���������� '):-!.
ten_to_word(7,'��������� '):-!.
ten_to_word(8,'����������� '):-!.
ten_to_word(9,'��������� '):-!.

hundred_to_word(0,''):-!.
hundred_to_word(1,'��� '):-!.
hundred_to_word(2,'������ '):-!.
hundred_to_word(3,'������ '):-!.
hundred_to_word(4,'��������� '):-!.
hundred_to_word(5,'������� '):-!.
hundred_to_word(6,'�������� '):-!.
hundred_to_word(7,'������� '):-!.
hundred_to_word(8,'��������� '):-!.
hundred_to_word(9,'��������� '):-!.

% converts number into list, comprised of its digits
num_to_list(0,[]):-!.
num_to_list(Num,List):- Digit is Num mod 10,
    Rest is Num div 10,
    num_to_list(Rest,List1),
    append(List1,[Digit],List),!.

% length of list
len([],0):-!.
len([_|Z],Y):-len(Z,Y1),Y is Y1+1.

% adds additional zeros into front of list to make it dividable by 3
format_list(List,List):-len(List,N),T is N mod 3, T = 0,!.
format_list(List,[0|List]):-len(List,N),T is N mod 3, T = 2,!.
format_list(List,[0,0|List]):-len(List,N),T is N mod 3, T = 1,!.

% writes number in list into io stream in its written variant
convert_list([],_):-!.
convert_list([Hundred,Ten,Digit|Rest],N):-
    Ten = 1,
    hundred_to_word(Hundred,Result_Hundred),
    write(Result_Hundred),
    ((Digit = 1,
    write('���������� '));
    (Digit = 2,
    write('���������� '))),
    write_delimiter(N,5),
    N1 is N-3,
    convert_list(Rest,N1),!.
convert_list([Hundred,Ten,Digit|Rest],N):-
    hundred_to_word(Hundred,Result_Hundred),
    write(Result_Hundred),
    ten_to_word(Ten,Result_Ten),
    write(Result_Ten),
    get_digit_form(N,Form),
    digit_to_word(Digit,Result_Digit,Form),
    write(Result_Digit),
    write_delimiter(N,Digit),
    N1 is N-3,
    convert_list(Rest,N1),!.

% sets word form if is of thousand
get_digit_form(6,f):-!.
get_digit_form(_,m):-!.

% writes thousand, million, etc.
write_delimiter(6,Last_Num):-
    ((Last_Num = 1,write('������ '));
    (Last_Num > 1, Last_Num<5,write('������ '));
    (Last_Num>=5,write('����� '))),!.
write_delimiter(9,Last_Num):-
    ((Last_Num = 1,write('������� '));
    (Last_Num > 1, Last_Num<5,write('�������� '));
    (Last_Num>=5,write('��������� '))),!.
write_delimiter(12,Last_Num):-
    ((Last_Num = 1,write('�������� '));
    (Last_Num > 1, Last_Num<5,write('��������� '));
    (Last_Num>=5,write('���������� '))),!.
write_delimiter(_,_):-!.

% assembels anwser
convert_number(Num):-num_to_list(Num,List1),format_list(List1,List),len(List,N),
   convert_list(List,N).


% file processing
file_to_list(F,L):-seen,see(F),readfile([],[],L).
readfile(T,R,L):-get0(X),(X<0,(T=[],L=R;
                         name(T1,T),append(R,[T1],L)),!;
                         (X=32;X=10;X=13),(T=[],readfile(T,R,L);
                         name(T1,T),append(R,[T1],R1),readfile([],R1,L)),!;
                         X>0,append(T,[X],T1),readfile(T1,R,L),!).


rewrite_list([]):-!.
rewrite_list([El|Rest]):-
    integer(El),
    convert_number(El),!,
    rewrite_list(Rest).
rewrite_list([El|Rest]):-
    write(El),
    write(' '),!,
    rewrite_list(Rest).

run(In_File,Out_File):-file_to_list(In_File,List),
    tell(Out_File),
    rewrite_list(List),
    told,!.
