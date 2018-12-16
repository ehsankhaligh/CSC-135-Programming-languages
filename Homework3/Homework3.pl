/*
Ehsan Hosseinzadeh Khaligh

Test Cases:

parent(john, fred).
parent(john, terry).
parent(terry, madge).
parent(fred, luther).
male(john).
male(fred).
male(luther).

uncle(U,V).
U = fred,
V = madge
---------------

cappedSum(7, [9,4,9,2,8], S).
S = 27

cappedSum(7, [6,4,9,2,8], S).
S = 26 .

---------------

reverseShuffle([3,8,2,6], [7,1,5,9], R).
[3, 9, 8, 5, 2, 1, 6, 7].

---------------

pingpong(P,I,N,G,O).
P = 3,
I = 8,
N = 2,
G = 1,
O = 0.

*/


/*------------------A-----------------------*/

/*
   child has's grandparents are persons's parents must be the same.
   Person must be a male.
*/

uncle(U,N) :- male(U), parent(Z,N), parent(G,Z), parent(G,U), Z\=U.

/*------------------B-----------------------*/

addTwoElm(X,Y, S):- S is X+Y.
cappedSum(N, [], 0).
cappedSum(CappNum, [X|Y], Sum) :-   X =< CappNum,  cappedSum(CappNum,Y, NewSum), addTwoElm(NewSum,X, Sum).
cappedSum(CappNum, [X|Y], Sum) :-   X > CappNum,  cappedSum(CappNum,Y, NewSum), addTwoElm(NewSum,CappNum, Sum).

/*------------------C-----------------------*/

/* reverse_list([1,2,3],[], X). X = [3, 2, 1]. */
reverse_list([],M,M).
reverse_list([Head|Tail],NewTail,M) :- reverse_list(Tail,[Head|NewTail], M).

/* forwardShuffle([3,8,2,6], [7,1,5,9], R). R = [3, 7, 8, 1, 2, 5, 6, 9|_6506].*/
forwardShuffle([],[],R).
forwardShuffle([X|Y], [H|T], [X,H | M]):- forwardShuffle(Y, T, M).

/* forwardShuffleBugFix([3, 7, 8, 1, 2, 5, 6, 9|_6506], L).  _6506 = [], L = [3, 7, 8, 1, 2, 5, 6, 9] .*/
forwardShuffleBugFix([],[]).
forwardShuffleBugFix([H|T1],[H|T2]) :- forwardShuffleBugFix(T1,T2).

/* reverseShuffle([3,8,2,6], [7,1,5,9], R). R = [3, 9, 8, 5, 2, 1, 6, 7]. */
reverseShuffle(L, M, R):- reverse_list(M,[],P), forwardShuffle(L, P, G), forwardShuffleBugFix(G, R).

/*------------------D-----------------------*/

numb(0).
numb(1).
numb(2).
numb(3).
numb(4).
numb(5).
numb(6).
numb(7).
numb(8).
numb(9).

pingpong(P,I,N,G,O) :-  numb(P), numb(I), numb(N), numb(G), numb(O),

                        P \= I, P \= N, P \= G, P \= O,
                        I \= N, I \= G, I \= O,
                        N \= G, N \= O,
                        G \= O,

                        FirstAdd is P * 100 + I * 10 + N ,
                        SecondAdd is I * 100 + N * 10 + G ,

                        Expected is FirstAdd + SecondAdd,
                        Result is G * 1000 + N * 100 + O * 10 + P,

                        Result = Expected.
