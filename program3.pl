% program3.pl
:- ['preamble.pl'].
:- ['lisp.pl'].
:- >>> 'Output of the following program :'.
:- >>> '[[defun,inc,[i],[add,i,1]],[defvar,x,0],[setq,x,[inc,x]]]'.

:- ([[defun,inc,[i],[add,i,1]],[defvar,x,0],[setq,x,[inc,x]]],s)-->>S,lookup(x,S,X),writeln(X),!.
