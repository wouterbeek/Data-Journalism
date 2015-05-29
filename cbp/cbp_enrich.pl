:- module(
  cbp_enrich,
  [
    enrich_with_bag/3 % +CbpGraph:atom
                      % +Cbp2BagGraph:atom
                      % +BagGraph:atom
  ]
).

/** <module> CBP enrich

@author Wouter Beek
@version 2015/05/29
*/

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plc(dcg/dcg_generics)).

:- use_module(plRdf(api/rdf_read)).

:- use_module(plSparql(sparql_build)).
:- use_module(plSparql(query/sparql_query_api)).

enrich_with_bag(G1, G2, G3):-
  gtrace,
  flag(number_of_enriched_entries, _, 0), %DEB
  rdf(_, cbpo:bezoekAdres, Address, G1), % NONDET
  enrich_with_bag(Address, G1, G2, G3),
  flag(number_of_enriched_entries, N, N + 1), %DEB
  debug(cbp, 'Enriched ~D entries.', [N]), %DEB
  fail.
enrich_with_bag(_, _, _).

enrich_with_bag(Address1, G1, G2, G3):-
  rdf(Address1, vcard:streetAddress, literal(type(_,StreetAddress)), G1),
  rdf(Address1, vcard:locality, literal(type(_,Locality)), G1),
  rdf(Address1, vcard:'postal-code', literal(type(_,PostalCode)), G1),
  find_bag_resource(StreetAddress, Locality, PostalCode, Address2),
  find_bag_triples(Address2, POs),
  rdf_assert(Address1, owl:sameAs, Address2, G2),
  maplist(assert_bag_triple(Address2, G3), POs).

assert_bag_triple(S, G, [P,O]):-
  rdf_assert(S, P, O, G).

find_bag_triples(S, POs):-
  atom_phrase(
    sparql_build(select, [], [p,o], [rdf(iri(S),var(p),var(o))], []),
    Q
  ),
  sparql_select0(Q, [POs], []).

find_bag_resource(StreetAddress, Locality, PostalCode, S):-
  atom_phrase(
    sparql_build(
      select,
      [vcard],
      [p,o],
      [
       rdf(var(s),vcard:streetAddress,var(a)),
       filter(strstarts(var(a),string(StreetAddress))),
       rdf(var(s),vcard:locality,var(b)),
       filter(strstarts(var(b),string(Locality))),
       rdf(var(s),vcard:'postal-code',var(c)),
       filter(strstarts(var(c),string(PostalCode)))
      ],
      []
    ),
    Q
  ),
  sparql_select0(Q, Rows, []),
  writeln(Rows),
  Rows = [S|_].

sparql_select0(Query, Rows, Options):-
  sparql_select('http://data.resc.info/bag/sparql', Query, Rows, Options).
