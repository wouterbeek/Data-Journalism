```prolog
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pairs)).

%! topk_verantwoordelijke(-Verantwoordelijke:iri, -Meldingen:nonneg) is multi.

topk_verantwoordelijke(V, N):-
    setof(V, M^rdf(M, cbpo:verantwoordelijke, V), Vs),
        maplist(number_of_meldingen, Vs, Ns),
	    pairs_keys_values(Pairs, Ns, Vs),
	       			     sort(1, @>=, Pairs, SortedPairs),
				         member(N-V, SortedPairs).

%! number_of_meldingen(+Verantwoordelijke:iri, -Meldingen:nonneg) is det.

number_of_meldingen(V, N):-
    setof(M, rdf(M, cbpo:verantwoordelijke, V), Ms),
        length(Ms, N).
```
