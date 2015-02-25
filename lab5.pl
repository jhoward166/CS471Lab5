/*
        CS471 - Programming Languages
        Assignment #5 due: 10-03-2014
        Author: Howard, Joseph (<EMail>)
        Date: 10-02-2014
 */

/*  Rename file lab5.pl */

/* 1:(10pts) Define a predicate "simplify/3" that succeeds if the last argument is 
      a list with items with the form Var:Value,  
      the first argument is a "var" atom in the list and 
      the second argument is the var's Value.  Requires only one clause.
      (hint::member, atom).
      ?- simplify(b,Value,[a:100,b:(-5)]).
      Value = -5
      ?- simplify(b,Value,[a : 1,b : 5]).
      Value = 5 .
      */
simplify(A,Value,D):- atom(A), member(A:B,D), Value = B.


      
 /* 2: (10pts).   In lab 4  problem 9, you wrote a predicate 'swap/2'.  What is the time complexity
                         of 'swap/2' ?  What are your assumptions?

The time complexity should be an average case of N where N is the number of elements. Each element is
checked to see if its a leaf, and has its children switched if its not. Checking to see if the element
is a leaf happens in constant time, and swapping the children happens in constant time. The function
then recursively descends to that elements children swaping them too. Constant time actions for each
element in the tree leaves you with an average case of O(N). 
     */ 

      

/* 3:  Syntax-Directed Differentiation:  A motivating example illustrating the 
         power of pattern matching in Prolog.
         Consider the following rules for symbolic differentiation
         (U, V are mathematical expressions, x is a variable):

        dx/dx = 1
        d(C)/x = 0.
        d(Cx)/x = C               (C is a constant)
        d(-U)/dx = -(dU/dx)
        d(U+V)/dx = dU/dx + dV/dx
        d(U-V)/dx = dU/dx - dV/dx
        d(U*V)/dx = U*(dV/dx) + V*(dU/dx)
        d(U^n)/dx = nU^(n-1)*(dU/dx)

        These rules can easily be translated into Prolog, for instance,
        the second rule can be defined as
                   d(C,x,0):-integer(C).
          and the fifth rule can be defined as
                   d(U+ V ,x, DU+ DV):-d(U,x,DU),d(V,x,DV).

         Write the remaining rules. Here is a test query:

            ?- d(3*(x +2*x*x),x,Result).
            Result = 3* (1+ (2*x*1+x*2))+ (x+2*x*x)*0 ;
            Result = 3* (1+ (2*x*1+x* (2*1+x*0)))+ (x+2*x*x)*0 ;
            false.

            ?- d(3*(x +2*x*x),x,Result).
            Result = 3* (1+ (2*x*1+x*2))+ (x+2*x*x)*0 .


        Keep in mind, though, that terms such as U+V are still trees with the
        functor at the root, and that evaluation of such terms requires 
        additional processing .  See next week's assignment.
        1 clause for each definition.
*/
d(x,x,1).
d(C,x,0):- number(C).
d(C,x,A):- C = A*x.
d((-C),x,A):- d(C,x,B), A = (-B).
d(U+ V,x, DU+DV):-d(U,x,DU),d(V,x,DV).
d(U- V,x, DU- DV):- d(U,x,DU), d(V,x,DV).
d(U*V,x, B + C):-B = U*DV, C = V*DU, d(U,x,DU), d(V,x,DV).
d(U^N,x,A*DU):- M is N-1, A=N*U^M, d(U,x,DU).

/* 4: Given a list of predicates, applylist(L) succeeds only if
      each of the predicates in the list succeeds. Note: the scope
      of variables names is the entire list. You can apply each predicate
      at the prompt to see how they work.  Make up your own. (2 lines)

   i.e.
      ?- applylist([=(A,5),is(B,+(4,5)),C is max(5,2),A=C]).
      A = 5,
      B = 9,
      C = 5.
      
      ?- applylist([=(A,5),is(B,+(4,5)),C is max(9,2),A=C]).
      false.
      
   2 clauses 
  */
applylist([]).
applylist([A|As]):- A , applylist(As).



/*5:(10 pts) Define a predicate append3DL  that concatenates three difference lists:
   ?- append3DL( [z,y|A] - A, [x,w | B] -B, [u,v | C] - C, What).
   A = [x, w, u, v|C],
   B = [u, v|C],
   What = [z, y, x, w, u, v|C]-C.

   ?- append3DL([1,2,3|X]-X,[a,b|Y]-Y,[x,y,z|Z]-Z,A - B).
   X = [a, b, x, y, z|B],
   Y = [x, y, z|B],
   Z = B,
   A = [1, 2, 3, a, b, x, y, z|B].

*/
%appendDL(L-M , M-N, L-N).
append3DL(A-B,B-C,C-D,A-D).



 %6. prodR(N+, ?P).
/* Given a number, N, P is a list of the product of the numbers from N
   down to 1 such that first number in P is the product of all the number from N to 1, 
   the second number in P the product of all the numbers from N-1 down to 1 etc.
   For example:
     
     ?- prodR(6,P).
     P =[720, 120, 24, 6, 2, 1] .
 
     ?- prodR(6,[720,120,10]).
     false.
     
     ?- prodR(6,[720, 120, 24, 6, 2, 1]).
     true. 
   
   2 clauses
*/ 

prodR(1,[1]).
prodR(A, [B1,B2|Bs]):- D is A - 1, prodR(D, [B2 | Bs]), B1 is B2 * A.


/** 7: In lab 4 your examined "Send more money" a well-known puzzle. Each of the letters
    D,E,M,N,O,R,S and Y represents a different digit. Moreover, when each
    letter is mapped to its corresponding digit the equation SEND + MORE =
    MONEY holds. Below is a very naive implementation. Since there are 8 letters 
    to be solved, it simply explore the 10*9*...*3 mappings of letters to
    digits. 
    
    A little insight can simplify things. Clearly, SEND < 9999 and 
    MORE < 9999. Thus MONEY < 19998 and hence M = 1. 
    Now we have SEND + 1ORE = 1ONEY. Again SEND < 9999 
     and now 1ORE < 1999 so 1ONEY < 11998. Since M is already bound to 1, 
     O must be bound to 0. A little more thought shows that S must be
     bound to 8 or 9, and that N = E + 1. Using these insights to reduce
     the number of solutions that must be explored, write a Prolog
     predicate soln([D,E,M,N,O,R,S,Y]) that solves this puzzle by binding
     the correct digits to each of the variables in the list. (Modified 
     from http://www.cs.wisc.edu/~fischer/)
    (1 clause with multiple subgoals.)
*/

solvSlow( [D,E,M,N,O,R,S,Y]) :-
	Lst = [S,E,N,D,M,O,R,Y],
	Digits = [0,1,2,3,4,5,6,7,8,9],
	assign_digits(Lst, Digits),
	M > 0, 
	S > 0,
	1000*S + 100*E + 10*N + D +
	1000*M + 100*O + 10*R + E =:=
	10000*M + 1000*O + 100*N + 10*E + Y,
	write(Lst).


assign_digits([], _List).
assign_digits([D|Ds], List):-
        select(D, List, NewList),
        assign_digits(Ds, NewList).

soln([D,E,M,N,O,R,S,Y]):-
	Lst = [S,E,N,D,M,O,R,Y],
        Digits = [0,1,2,3,4,5,6,7,8,9],
        assign_digits(Lst, Digits),
	M = 1,
	O = 0,
	S < 10,
	S > 7,
	N is E+1,
	1000*S + 100*E + 10*N + D +
        1000*M + 100*O + 10*R + E =:=
        10000*M + 1000*O + 100*N + 10*E + Y,
        write(Lst).

