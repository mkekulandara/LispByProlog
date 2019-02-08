% lisp.pl
% Version 1.0
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% definition of our language :
%
%	Unit ::= [ SFormList ] 
%		| []
%
%	SFormList ::= SForm 
%		| SForm , SFormList
%
%	SForm ::= [ defvar , x , SExp ]
%		| [ setq , x , SExp ]
%		| [ defun , f , [ FL ] , SExp ]
%		| [ defun , f , [ ] , SExp ]
%
%	SExp ::= n
%		| x
%		| true
%		| false
%		| [ add , SExp , SExp , ... ]
%		| [ sub , SExp , SExp ]
%		| [ mult , SExp , SExp , ... ]
%		| [ eq , SExp , SExp ]
%		| [ le , SExp , SExp ]
%		| [ neg , SExp ]
%		| [ and , SExp , SExp , ... ]
%		| [ or , SExp , SExp , ... ]
%		| [ if , SExp , SExp, SExp ]
%		| [ let , x , SExp , SExp ]
%		| [ f ]
%		| [ f , SExp , ... ]
%
%	FL ::= x , FL
%		| x
%
%
% for convenience sake make seq infix and left associative
:- op(1200,yfx,seq).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% some basic definitions
:- ['preamble.pl'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% we need the definition of predicate 'xis' for the 
% evaluation of our terms.
:- ['xis.pl'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% semantic definition of the forth commands

([],Env) -->> Env :- !.

([Instr|P],Env) -->> OEnv :- 
	(Instr,Env) -->> IEnv,
	(P,IEnv) -->> OEnv,!.

(N,Env) -->> (Val,Env) :- 
	int(N),
	Val xis N,!.

([defvar,X,SExp],Env)-->>OEnv :-
	(SExp,Env) -->> (ValX,IEnv),
	declarevar(X,ValX,IEnv,OEnv),!.
	
([setq,X,SExp],Env)-->>OEnv :-
	lookup(X,Env,_),
	(SExp,Env) -->> (ValX,IEnv),
	bindval(X,ValX,IEnv,OEnv),!.
	
([defun,F,FL,SExp],Env)-->>OEnv :-
	declarevar(F,funval(FL,SExp),Env,OEnv),!.
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

([add,SExpA,SExpB],Env) -->>(Val,OEnv) :-
	(SExpA,Env) -->> (ValA,IEnv),
	(SExpB,IEnv) -->> (ValB,OEnv),
	Val xis (ValA + ValB),!.

([add,SExpA,SExpB|List],Env) -->>(Val,OEnv) :-
	(SExpA,Env) -->> (ValA,IEnv),
	([add,SExpB|List],IEnv) -->> (ValB,OEnv),
	Val xis (ValA + ValB),!.
	
([sub,SExpA,SExpB],Env) -->>(Val,OEnv) :-
	(SExpA,Env) -->> (ValA,IEnv),
	(SExpB,IEnv) -->> (ValB,OEnv),
	Val xis (ValA - ValB),!.
	
([mult,SExpA,SExpB],Env) -->>(Val,OEnv) :-
	(SExpA,Env) -->> (ValA,IEnv),
	(SExpB,IEnv) -->> (ValB,OEnv),
	Val xis (ValA * ValB),!.

([mult,SExpA,SExpB|List],Env) -->>(Val,OEnv) :-
	(SExpA,Env) -->> (ValA,IEnv),
	([mult,SExpB|List],IEnv) -->> (ValB,OEnv),
	Val xis (ValA * ValB),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(true,Env) -->> (true,Env) :- !.                 

(false,Env) -->> (false,Env) :- !.

([eq,SExpA,SExpB],Env) -->> (Val,OEnv) :-
	(SExpA,Env) -->> (ValA,IEnv),
	(SExpB,IEnv) -->> (ValB,OEnv),
	Val xis (ValA =:= ValB),!.
	
([le,SExpA,SExpB],Env) -->> (Val,OEnv) :-
	(SExpA,Env) -->> (ValA,IEnv),
	(SExpB,IEnv) -->> (ValB,OEnv),
	Val xis (ValA =< ValB),!.
	
([neg,SExpA],Env) -->> (Val,OEnv) :-
	(SExpA,Env) -->> (ValA,OEnv),
	Val xis (not ValA),!.

([and,SExpA,SExpB],Env) -->>(Val,OEnv) :-
	(SExpA,Env) -->> (ValA,IEnv),
	(SExpB,IEnv) -->> (ValB,OEnv),
	Val xis (ValA and ValB),!.
	
([and,SExpA,SExpB|List],Env) -->>(Val,OEnv) :-
	(SExpA,Env) -->> (ValA,IEnv),
	([and,SExpB|List],IEnv) -->> (ValB,OEnv),
	Val xis (ValA and ValB),!.
	
([or,SExpA,SExpB],Env) -->> (Val,OEnv) :-
	(SExpA,Env) -->> (ValA,IEnv),
	(SExpB,IEnv) -->> (ValB,OEnv),
	Val xis (ValA or ValB),!.

([or,SExpA,SExpB|List],Env) -->> (Val,OEnv) :-
	(SExpA,Env) -->> (ValA,IEnv),
	([or,SExpB|List],IEnv) -->> (ValB,OEnv),
	Val xis (ValA or ValB),!.	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
([let,X,SExpA,SExpB],Env) -->> (Val,OEnv) :-      
    bindval(X,SExpA,Env,LEnv),
	(SExpB,LEnv)-->>(Val,OEnv),!.
	
([if,B,C0,_],Env) -->> (Val,OEnv) :-              % if true
    (B,Env) -->> (true,IEnv),
    (C0,IEnv) -->> (Val,OEnv),!.

([if,B,_,C1],Env) -->> (Val,OEnv) :-              % if false
    (B,Env) -->> (false,IEnv),
    (C1,IEnv) -->> (Val,OEnv),!.	

([F],Env) -->> (Val,OEnv) :-
	lookup(F,Env,funval([],SExp)),
	(SExp,Env) -->> (Val,OEnv),!.
	
([F|PList],Env) -->> (Val,OEnv) :-  
	lookup(F,Env,funval(FL,SExp)),
	declareparams(FL,PList,Env,LEnv),
	(SExp,LEnv)-->>(Val,OEnv),!.

(X,Env) -->> (Val,Env) :-
    atom(X),
	lookup(X,Env,Val),!.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the predicate 'lookup(+Variable,+State,-Value)' looks up
% the variable in the state and returns its bound value.
:- dynamic lookup/3.                % modifiable predicate

lookup(_,s0,_) :- fail.

lookup(X,env([],S),Val) :-
    lookup(X,S,Val).

lookup(X,env([bind(Val,X)|_],_),Val).

lookup(X,env([_|Rest],S),Val) :- 
    lookup(X,env(Rest,S),Val).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the predicate 'bindval(+Variable,+Value,+State,-FinalState)' updates
% a binding term in the state.  this update is done "in place"
% in order to support global variables.  the predicate has to
% search both the binding list and the stack of binding
% lists.
:- dynamic bindval/4.                   % modifiable predicate

bindval(_,_,s0,_) :- 
    fail.

bindval(X,Val,env([],S),env([],NewS)) :-
    bindval(X,Val,S,NewS).

bindval(X,Val,env([bind(_,X)|L],S),env([bind(Val,X)|L],S)).

bindval(X,Val,env([bind(V,Y)|L],S),env([bind(V,Y)|NewL],NewS)) :-
    bindval(X,Val,env(L,S),env(NewL,NewS)).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the predicate 'declarevar(+Variable,+Val,+State,-FinalState)' declares
% a variable by inserting a new binding term into the current
% environment.
:- dynamic declarevar/4.                   % modifiable predicate

declarevar(X,V,S,env([bind(V,X)],S)) :-
    atom(S),!.

declarevar(X,V,env(L,S),env([bind(V,X)|L],S)).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the predicate 'declareparams(+ActualList,+State,-FinalState)' declares
% a non-empty list of formal variables by inserting new binding terms into 
% the current environment.
:- dynamic declareparams/4.                   % modifiable predicate

% Note: positional correspondence can easily be implemented by
% co-recusion onf the formal parameter list

declareparams([X],[A],State,OState) :-
    (A,State)-->>(ValA,S1),               
    declarevar(X,ValA,S1,OState).

declareparams([X|TX],[A|TA],State,OState) :-
    (A,State)-->>(ValA,S1),                
    declarevar(X,ValA,S1,S2),
    declareparams([TX],[TA],S2,OState).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the predicate 'pushenv(+State,-FinalState)' pushes
% a new binding term list on the stack
:- dynamic pushenv/2.

pushenv(S,env([],S)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the predicate 'popenv(+State,-FinalState)' pops
% a  binding term list off the stack
:- dynamic popenv/2.

popenv(env(_,S),S).