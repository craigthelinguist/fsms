

:- [fsms].

% The Miller-Rabin powerset construction.
% Converts an instance of an NDFA into a DFA which accepts the same language.

%

% Return the set of nodes reachable from State via a single given Trans.
adjacent(Graph, State, Trans, AdjacentSet) :-
   setof(Reachable,
         member(edge(State, Trans, Reachable), Graph),
         AdjacentSet), !.

% If the above version fails, return the empty list.
adjacent(_, _, _, []) :- !.





% Return the epsilon closure of a set of nodes.
epsilonClosureOf(_, [], []).

epsilonClosureOf(Graph, [N|Nodes], TheClosure) :-
   epsilonClosureOf(Graph, Nodes, Closure2),
   epsilonClosure(Graph, N, Closure),
   append(Closure, Closure2, TheClosure).

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




postAndClosure(_, [], []).

postAndClosure(Graph1, [N|Nodes], Graph2) :-

   % Get outgoing transitions from current node N.
   outgoing(Graph1, N, Transitions),

   % Find the set reachable by a single transition and any number
   % of epsilon transitions. For each such transition we get an
   % edge in the new graph.
   findall(
      edge(N, Trans, ClozureNode),
      (
         member(Trans, Transitions),
         adjacent(Graph1, N, Trans, Adj),
         epsilonClosureOf(Graph1, Adj, Clozure),
         closure2node(Clozure, ClozureNode)
      ),
      AllClozures),

   % Append the above to the result of taking postAndClosure on the
   % rest of the nodes.
   postAndClosure(Graph1, Nodes, Subgraph),
   append(AllClozures, Subgraph, Graph2).
   




% A DFA is already a DFA.
nfa2dfa(Nfa, toDo:toDo:Graph2) :-
   graph(Nfa, Graph),
   nodes(Graph, Nodes),
   postAndClosure(Graph, Nodes, Graph2).



testMachine(a:e:[edge(a, 1, b), edge(b, 2, a), edge(a, 1, c), edge(c, 2, e), edge(e, 3, a)]).






   


