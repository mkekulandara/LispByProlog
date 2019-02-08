% program4.pl
:- ['preamble.pl'].
:- ['lisp.pl'].
:- >>> 'Output of the following program :'.
:- >>> '[[defun,fact,[i],[if,[eq,i,1],1,[mult,i,[fact,[sub,i,1]]]]],[defvar,x,[fact,3]]]'.

:- ([[defun,fact,[i],[if,[eq,i,1],1,[mult,i,[fact,[sub,i,1]]]]],[defvar,y,3],[defvar,x,[fact,y]]],s)-->>S,lookup(x,S,X),writeln(X),!.
