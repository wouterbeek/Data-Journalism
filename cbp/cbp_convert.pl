:- module(
  cbp,
  [
    convert_cbp/2 % +M, +G
  ]
).

/** <module> CBP

@author Wouter Beek
@version 2015/05/23, 2016/05
*/

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(dict_ext)).
:- use_module(library(http/http_download)).
:- use_module(library(print_ext)).
:- use_module(library(q/qb)).
:- use_module(library(q/q_stmt)).
:- use_module(library(rdfs/rdfs_ext)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(yall)).

:- rdf_meta
   convert_cbp(+, r).





convert_cbp(M, G) :-
  % Reset progress tracker.
  flag(cbp_entries, _, 0),

  % Input file.
  absolute_file_name(data('cbp.tar.gz'), File, [access(read)]),
  forall(json_download(File, Json),
    maplist({M,G}/[Entry]>>assert_entry(M, Entry, G), Json)
  ).



% @tbd Assert entry.
assert_entry(M, D, G) :-
  cbp_agent(M, D.name, Agent, G),
  dict_pairs(D.meldingen, Pairs),
  maplist({M,Agent,G}/[Pair]>>assert_melding_pair(M, Agent, Pair, G), Pairs), !,
  print_progress(100, G).
assert_entry(M, D, G) :-
  gtrace, %DEB
  print_dict(D),
  assert_entry(M, D, G).



assert_melding_pair(M, Agent, Id-D, G) :-
  qb_iri(cbpr, Melding),
  qb_instance(M, Melding, cbpo:'Melding', G),
  qb(M, Melding, cbpo:id, Id^^xsd:string, G),
  qb(M, Melding, cbpo:melder, Agent, G),
  ignore(assert_betrokkene(M, Melding, D.get(betrokkenen), G)),
  qb(M, Melding, dcterms:description, D.description@nl, G),
  maplist({M,Melding,G}/[Doel]>>assert_doel(M, Melding, Doel, G), D.doelen),
  ignore(qb(M, Melding, cbpo:doorgifteBuitenEU, D.doorgifte_buiten_eu^^xsd:boolean, G)),
  qb(M, Melding, cbpo:naamVerwerking, D.naam_verwerking^^xsd:string, G),
  ignore(
    maplist(
      {M,Melding,G}/[Ontvanger]>>assert_ontvanger(M, Melding, Ontvanger, G),
      D.get(ontvangers)
    )
  ),
  maplist(
    {M,Melding,G}/[Verantw]>>assert_verantwoordelijke(M, Melding, Verantw, G),
    D.verantwoordelijken
  ).



assert_betrokkene(M, Melding, D, G) :-
  dict_pairs(D, Pairs),
  maplist({M,Melding,G}/[Pair]>>assert_betrokkene_pair(M, Melding, Pair, G), Pairs).



% @tbd Assert properties.
assert_betrokkene_pair(M, Melding, Betrokkene-_Properties, G) :-
  cbp_agent(M, Betrokkene, Agent, G),
  qb(M, Melding, cbpo:betrokkene, Agent, G).



assert_doel(M, Melding, Doel, G) :-
  qb(M, Melding, cbpo:doel, Doel^^xsd:string, G).



assert_ontvanger(M, Melding, Ontvanger, G) :-
  cbp_agent(M, Ontvanger, Agent, G),
  qb(M, Melding, cbpo:ontvanger, Agent, G).



assert_verantwoordelijke(M, Melding, D, G) :-
  cbp_agent(M, D.'Naam', Agent, G),
  qb(M, Agent, cbpo:bezoekAdres, D.'Bezoekadres'^^xsd:string, G),
  ignore(qb(M, Agent, cbpo:postAdres, D.get('Postadres')^^xsd:string, G)),
  qb(M, Melding, cbpo:verantwoordelijke, Agent, G).



cbp_agent(M, Name, Agent, _) :-
  q_pref_label(M, Agent, Name), !.
cbp_agent(M, Name, Agent, G) :-
  qb_iri(cbpr, Agent),
  qb_instance(M, Agent, foaf:'Agent', G),
  qb_label(M, Agent, Name@nl, G).





% DEBUG %

print_progress(Step, G) :-
  flag(cbp_entries, N, N + 1),
  (   N mod Step =:= 0,
      N > 0
  ->  rdf_statistics(triples_by_graph(G,T)),
      debug(cbp, "~D entries have been converted into ~D triples.", [N,T])
  ;   true
  ).
