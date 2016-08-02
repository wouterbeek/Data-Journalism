:- module(
  cbp_enrich,
  [
    enrich_with_bag/4 % +M, +CbpG, +Cbp2BagG, +BagG
  ]
).

/** <module> CBP enrichment

@author Wouter Beek
@version 2015/05/29, 2016/05
*/

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(q/qb)).
:- use_module(library(q/q_stmt)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(sparql/sparql_query)).





enrich_with_bag(M, G1, G2, G3):-
  flag(number_of_enriched_entries, _, 0), %DEB
  q(M, _, cbpo:bezoekAdres, Address, G1), % NONDET
  enrich_with_bag(M, Address, G1, G2, G3),
  flag(number_of_enriched_entries, N, N + 1), %DEB
  debug(cbp, "Enriched ~D entries.", [N]), %DEB
  fail.
enrich_with_bag(_, _, _, _).



enrich_with_bag(M, Address1, G1, G2, G3):-
  q(M, Address1, vcard:streetAddress, StreetAddress^^_, G1),
  q(M, Address1, vcard:locality, Locality^^_, G1),
  q(M, Address1, vcard:'postal-code', PostalCode^^_, G1),

  % SPARQL find subject.
  atom_phrase(
    sparql_build_select(
      [vcard],
      [p,o],
      [
       rdf(var(s),vcard:streetAddress,var(a)),
       filter(strstarts(var(a),string(StreetAddress))),
       rdf(var(s),vcard:locality,var(b)),
       filter(strstarts(var(b),string(Locality))),
       rdf(var(s),vcard:'postal-code',var(c)),
       filter(strstarts(var(c),string(PostalCode)))
      ]
    ),
    Q
  ),
  sparql_select('http://data.resc.info/bag/sparql', Q, [S|_]),
  
  % SPARQL find predicate-object pairs.
  atom_phrase(
    sparql_build(select, [], [p,o], [rdf(iri(Address2),var(p),var(o))], []),
    Q
  ),
  sparql_select0(Q, [POs], []),
  
  qb_identity(M, Address1, Address2, G2),
  maplist({Address2,G3}/[[P,O]]>>qb(M, Address2, P, O, G3), POs).
