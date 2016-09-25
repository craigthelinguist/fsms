

% A graph is represented as a list of labelled, directed edges.
% Example: [edge(a, 1, b), edge(a, 1, c)]

% Gather up the edges in a graph.
edges(Graph, Events) :-
   bagof(Event, S1^S2^member(edge(S1, Event, S2), Graph), Events).

% Gather up the nodes in a graph.
nodes(Graph, States) :-
   setof(S1,
         E^S2^(member(edge(S1, E, S2), Graph) ; member(edge(S2, E, S1), Graph)),
         States).

% An edge is parallel if it occurs more than once in the Graph.
parallelEdge(Graph, edge(A, E, B)) :-
   member(edge(A, E, B), Graph),
   count(edge(A, E, B), Graph, X),
   X > 1.




% An FSM is represented as a triple of the form (start state : end state : graph).
% These predicates extract the components of an FSM.
start((Start:_:_), Start).
finish((_:Finish:_), Finish).
graph((_:_:Graph), Graph).

% This is our representation of the special "epsilon-transition".
epsilon(epsilon).

% A FSM is non-deterministic if its graph contains a parallel edge.
ndfa(FSM) :-
   graph(FSM, Graph),
   parallelEdge(Graph, _), !.

% A FSM is also non-deterministic if it contains two (distinct) edges with the same
% source node and event.
ndfa(FSM) :-
   graph(FSM, Graph),
   member(edge(S, E, X), Graph),
   member(edge(S, E, Y), Graph),
   X \= Y, !.

% A Graph is deterministic iff it is not non-deterministic.
dfa(FSM) :-
   graph(FSM, Graph),
   not(ndfa(Graph)).

% An FSM accepts a String if there is an accepting path along its graph which consumes
% each token in the String.
accepts(FSM, String) :-
   start(FSM, State),
   acceptingPath(FSM, State, String).

% The empty string is accepting if you're in the finishing state.
acceptingPath(FSM, State, []) :-
   finish(FSM, State).

% A non-empty string is accepting if you can take a transition matching the next event
% in the string, and the rest of the string is accepting from the new state.
acceptingPath(FSM, State1, [NextEvent | Rest]) :-
   graph(FSM, Graph),
   member(edge(State1, NextEvent, State2), Graph),
   acceptingPath(FSM, State2, Rest).

% The language of an FSM is the set of all accepted strings.
language(FSM, Language) :-
   findall(String, accept(FSM, String), Language).






% Count number of occurrences of an item in a list.
count(_, [], 0) :- !.

count(X, [X|L], Sum) :-
   count(X, L, SubSum),
   Sum is 1 + SubSum, !.

count(X, [Y|L], Sum) :-
   X \= Y,
   count(X, L, Sum), !.








