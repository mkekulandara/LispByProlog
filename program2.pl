% program2.pl
:- ['preamble.pl'].
:- ['lisp.pl'].
:- >>> 'Output of the following program :'.
:- >>> '[[defvar,x,1],[setq,x,[add,x,1]]]'.

:- ([[defvar,x,1],[setq,x,[add,x,1]]],s)-->>S,lookup(x,S,X),writeln(X),!.