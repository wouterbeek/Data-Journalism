:- module(cbp, [convert_cbp/0, convert_cbp/1]).

/** <module> CBP

@author Wouter Beek
@version 2015/05/23, 2016/05
*/

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(dict_ext)).
:- use_module(library(http/http_download)).
:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdfs/rdfs_ext)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(yall)).





convert_cbp:-
  thread_create(convert_cbp(cbp), _, [detached(true)]).


convert_cbp(G) :-
  % Reset progress tracker.
  flag(cbp_entries, _, 0),

  % Input file.
  absolute_file_name(data('cbp.tar.gz'), File, [access(read)]),
  forall(json_download(File, Json),
    maplist({G}/[Entry]>>assert_entry(Entry, G), Json)
  ).



% @tbd Assert entry.
assert_entry(D, G) :-
  cbp_agent(D.name, Agent, G),
  dict_pairs(D.meldingen, Pairs),
  maplist({Agent,G}/[Pair]>>assert_melding_pair(Agent, Pair, G), Pairs), !,
  print_progress(100, G).
assert_entry(D, G) :-
  gtrace,
  print_dict(D),
  assert_entry(D, G).



assert_melding_pair(Agent, Id-D, G) :-
  rdf_create_iri(cbpr, Melding),
  rdf_assert_instance(Melding, cbpo:'Melding', G),
  rdf_assert(Melding, cbpo:id, Id^^xsd:string, G),
  rdf_assert(Melding, cbpo:melder, Agent, G),
  ignore(assert_betrokkene(Melding, D.get(betrokkenen), G)),
  rdf_assert(Melding, dcterms:description, D.description@nl, G),
  maplist({Melding,G}/[Doel]>>assert_doel(Melding, Doel, G), D.doelen),
  ignore(rdf_assert(Melding, cbpo:doorgifteBuitenEU, D.doorgifte_buiten_eu^^xsd:boolean, G)),
  rdf_assert_string(Melding, cbpo:naamVerwerking, D.naam_verwerking, G),
  ignore(maplist({Melding,G}/[Ontvanger]>>assert_ontvanger(Melding, Ontvanger, G), D.get(ontvangers))),
  maplist({Melding,G}/[Verantw]>>assert_verantwoordelijke(Melding, Verantw, G), D.verantwoordelijken).



assert_betrokkene(Melding, D, G) :-
  dict_pairs(D, Pairs),
  maplist({Melding,G}/[Pair]>>assert_betrokkene_pair(Melding, Pair, G), Pairs).



% @tbd Assert properties.
assert_betrokkene_pair(Melding, Betrokkene-_Properties, G) :-
  cbp_agent(Betrokkene, Agent, G),
  rdf_assert(Melding, cbpo:betrokkene, Agent, G).



assert_doel(Melding, Doel, G) :-
  rdf_assert(Melding, cbpo:doel, Doel^^xsd:string, G).



assert_ontvanger(Melding, Ontvanger, G) :-
  cbp_agent(Ontvanger, Agent, G),
  rdf_assert(Melding, cbpo:ontvanger, Agent, G).



assert_verantwoordelijke(Melding, D, G) :-
  cbp_agent(D.'Naam', Agent, G),
  rdf_assert(Agent, cbpo:bezoekAdres, D.'Bezoekadres'^^xsd:string, G),
  ignore(rdf_assert(Agent, cbpo:postAdres, D.get('Postadres')^^xsd:string, G)),
  rdf_assert(Melding, cbpo:verantwoordelijke, Agent, G).



cbp_agent(Name, Agent, _) :-
  rdfs_pref_label(Agent, Name), !.
cbp_agent(Name, Agent, G) :-
  rdf_create_iri(cbpr, Agent),
  rdf_assert_instance(Agent, foaf:'Agent', G),
  rdfs_assert_label(Agent, Name@nl, G).





% DEBUG %

print_progress(Step, G) :-
  flag(cbp_entries, N, N + 1),
  (   N mod Step =:= 0,
      N > 0
  ->  rdf_statistics(triples_by_graph(G,T)),
      debug(cbp, "~D entries have been converted into ~D triples.", [N,T])
  ;   true
  ).
