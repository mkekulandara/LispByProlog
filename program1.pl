% program1.pl
:- ['preamble.pl'].
:- ['lisp.pl'].
:- >>> 'Output of the following program :'.
:- >>> '[add,1,2]'.


:- ([add,1,2],s)-->>(S),writeln(S),!.