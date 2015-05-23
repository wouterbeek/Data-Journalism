:- module(cbp, [load/0]).

/** <module> CBP

@author Wouter Beek
@version 2015/05/23
*/

:- use_module(library(apply)).
:- use_module(library(archive)).
:- use_module(library(http/json)).
:- use_module(library(lambda)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(semweb/rdfs)).

:- use_module(plc(dcg/dcg_pl_term)).
:- use_module(plc(generics/code_ext)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdfs_build)).

:- rdf_register_prefix(cbpo, 'http://www.cbp.nl/ontology/').
:- rdf_register_prefix(cbpr, 'http://www.cbp.nl/resource/').

load:-
  thread_create(load(cbp), _, [detached(true)]).

load(G):-
  reset_progress,
  absolute_file_name(data('cbp.tar.gz'), File, [access(read)]),
  setup_call_cleanup(
    archive_open(File, Archive, []),
    load_archive(Archive, G),
    archive_close(Archive)
  ).

load_archive(Archive, G):-
  repeat,
  archive_data_stream(Archive, In, [meta_data([H|_])]),
  call_cleanup(
    load_stream(In, H.name, G),
    close(In)
  ),
  fail.

load_stream(In, LocalName, G):-
  file_name_extension(_, Ext, LocalName),
  (   Ext == json
  ->  load_json(In, G)
  ;   print_message(informational, non_json_entry(LocalName))
  ).

load_json(In, G):-
  json_read_dict0(In, D),
  maplist(\Entry^assert_entry(Entry, G), D).

% @tbd Assert entry.
assert_entry(D, G):-
  cbp_agent(D.name, Agent, G),
  dict_pairs(D.meldingen, _, Pairs),
  maplist(\Pair^assert_melding_pair(Agent, Pair, G), Pairs), !,
  print_progress.
assert_entry(D, G):-
  gtrace,
  print_dict(D),
  assert_entry(D, G).

assert_melding_pair(Agent, Id-D, G):-
  rdf_create_next_resource(cbpr, [], cbpo:'Melding', G, Melding),
  rdf_assert_string(Melding, cbpo:id, Id, G),
  rdf_assert(Melding, cbpo:melder, Agent, G),
  ignore(assert_betrokkene(Melding, D.get(betrokkenen), G)),
  rdf_assert_langstring(Melding, dcterms:description, [nl]-D.description, G),
  maplist(\Doel^assert_doel(Melding, Doel, G), D.doelen),
  ignore(
    rdf_assert_typed_literal(
      Melding,
      cbpo:doorgifteBuitenEU,
      D.doorgifte_buiten_eu,
      xsd:boolean,
      G
    )
  ),
  rdf_assert_string(Melding, cbpo:naamVerwerking, D.naam_verwerking, G),
  ignore(
    maplist(
      \Ontvanger^assert_ontvanger(Melding, Ontvanger, G),
      D.get(ontvangers)
    )
  ),
  maplist(
    \Verantwoordelijke^assert_verantwoordelijke(Melding, Verantwoordelijke, G),
    D.verantwoordelijken
  ).

assert_betrokkene(Melding, D, G):-
  dict_pairs(D, _, Pairs),
  maplist(\Pair^assert_betrokkene_pair(Melding, Pair, G), Pairs).

% @tbd Assert properties.
assert_betrokkene_pair(Melding, Betrokkene-_Properties, G):-
  cbp_agent(Betrokkene, Agent, G),
  rdf_assert(Melding, cbpo:betrokkene, Agent, G).

assert_doel(Melding, Doel, G):-
  rdf_assert_string(Melding, cbpo:doel, Doel, G).

assert_ontvanger(Melding, Ontvanger, G):-
  cbp_agent(Ontvanger, Agent, G),
  rdf_assert(Melding, cbpo:ontvanger, Agent, G).

assert_verantwoordelijke(Melding, D, G):-
  cbp_agent(D.'Naam', Agent, G),
  rdf_assert_string(Agent, cbpo:bezoekAdres, D.'Bezoekadres', G),
  ignore(rdf_assert_string(Agent, cbpo:postAdres, D.get('Postadres'), G)),
  rdf_assert(Melding, cbpo:verantwoordelijke, Agent, G).

cbp_agent(Name, Agent, _):-
  rdfs_label(Agent, Name), !.
cbp_agent(Name, Agent, G):-
  rdf_create_next_resource(cbpr, [], foaf:'Agent', G, Agent),
  rdfs_assert_label(Agent, [nl]-Name, G).



%  HELPERS %

json_read_dict0(In, D):-
  json_read_dict(In, D0),
  string_atom_term(D0, D).

print_dict(D):-
  phrase(dcg_pl_term(D), Cs),
  put_codes(Cs),
  nl.

print_pair(N-V):-
  format(current_output, '~w\t=\t~w\n', [N,V]).

print_progress:-
  flag(cbp_entries, N, N + 1).

reset_progress:-
  flag(cbp_entries, _, 0).

string_atom_term(N1-V1, N2-V2):- !,
  maplist(string_atom_term, [N1,V1], [N2,V2]).
string_atom_term(L1, L2):-
  is_list(L1), !,
  maplist(string_atom_term, L1, L2).
string_atom_term(D1, D2):-
  is_dict(D1), !,
  dict_pairs(D1, X, L1),
  maplist(string_atom_term, L1, L2),
  dict_pairs(D2, X, L2).
string_atom_term(S, A):-
  string(S), !,
  atom_string(A, S).
string_atom_term(T, T).



% MESSAGES %

:- multifile(prolog:message//1).

prolog:message(non_json_entry(LocalName)) -->
  ['Entry ',LocalName,' does not have the JSON file extension.'].
