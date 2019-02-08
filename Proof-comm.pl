% prove that [add,a,b]~[add,b,a]
% with a and b either variables or integer values.
%
% show that
% (forall e,a,b,s)(exists V0,V1)
%    [([add,a,b],s0)-->>(V1,_) ^
%     ([add,b,a],s0)-->>(V2,_) ^
%     V1=V2]
% assuming
% (a,Env) -->> va.
% (b,Env) -->> vb.
% 

% load semantics
:- ['lisp.pl'].

% assumptions
:- asserta((a,s0) -->> va).
:- asserta((b,s0) -->> vb).
% we also have to assume that the values are integers for add's sake'
:- asserta(int(va)).
:- asserta(int(vb)).

% proof
:- ([add,1,2],s0)-->>(V1,_),([add,2,1],s0)-->>(V2,_),V1=V2,!.