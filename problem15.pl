% -*-Prolog-*-

:- dynamic grid/3.

grid(0,_,1).
grid(_,0,1).
grid(N,M,X) :- N1 is N-1,
               M1 is M-1,
              grid(N1,M,Y),
              grid(N,M1,Z),
	          X is Y+Z,
              asserta(grid(N,M,X)).

:- grid(20,20,X), display(X).
