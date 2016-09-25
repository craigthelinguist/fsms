

:- [fsms].

% The Miller-Rabin powerset construction.
% Converts an instance of an NDFA into a DFA which accepts the same language.

%
 nfa2dfa(Ndfa, Dfa) :-

% Return the set of nodes reachable from State via a single given Trans.
adjacent(Graph, State, Trans, AdjacentSet) :-
   setof(Reachable,
         member(edge(State, Trans, Reachable), Graph),
         AdjacentSet), !.

% If the above version fails, return the empty list.
adjacent(_, _, _, []) :- !.

% Return the set of nodes reachable from State via 0 or more epsilon transitions.
epsilonClosure(Graph, State, Closure) :-
   adjacent(Graph, State, epsilon, Neighbours),
   closure(Graph, Neighbours, [State], Closure).

% If no more nodes to visit then everything you've visited is the closure.
closure(_, [], Closure, Closure) :- !.

% If you already visited the next node then discard it.
closure(Graph, [N|ToVisit], Visited, Closure) :-
   member(N, Visited),
   closure(Graph, ToVisit, Visited, Closure), !.

% Find all nodes reachable from a single epsilon transition and add them to the
% visited set, producing Visited2. Recursively compute closure.
closure(Graph, [N|ToVisit], Visited, Closure) :-
   adjacent(Graph, N, epsilon, Adj),
   append(ToVisit, Adj, ToVisit2),
   closure(Graph, ToVisit2, [N|Visited], Closure).

% Turn a set of nodes into a single node, represented by String.
closure2node(Closure, String) :-
   sort(Closure, Closure2),
   closure2node2(Closure2, String).

closure2node2([X], X) :- !.
closure2node2([X|Xs], Rest) :-
   closure2node2(Xs, Rest2),
   Rest = X:Rest2.

   


