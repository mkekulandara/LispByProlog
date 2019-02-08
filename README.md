# LispByProlog
Interpretation of basic functionality of the Lisp programming language in Prolog

Unit ::= [ SFormList ] | []
SFormList ::= SForm | SForm , SFormList
SForm ::= [ defvar , x , SExp ]
| [ setq , x , SExp ]
| [ defun , f , [ FL ] , SExp ]
| [ defun , f , [ ] , SExp ]
SExp ::= n
| x
| true
| false
| [ add , SExp , SExp , ... ]
| [ sub , SExp , SExp ]
| [ mult , SExp , SExp , ... ]
| [ eq , SExp , SExp ]
| [ le , SExp , SExp ]
| [ neg , SExp ]
| [ and , SExp , SExp , ... ]
| [ or , SExp , SExp , ... ]
| [ if , SExp , SExp, SExp ]
| [ let , x , SExp , SExp ]
| [ f ]
| [ f , SExp , ... ]
FL ::= x , FL
| x
