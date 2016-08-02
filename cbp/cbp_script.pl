:- module(cbp_script, [cbp_script/0]).

/** <module> CBP script

Runs a script that loads and enriches the CBP dataset.

@author Wouter Beek
@version 2015/05/29, 2016/05
*/

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf11)).

:- use_module(cbp_convert).
:- use_module(cbp_enrich).
:- use_module(cbp_structure).

:- qb_alias(cbpo, 'http://www.cbp.nl/ontology/').
:- qb_alias(cbpr, 'http://www.cbp.nl/resource/').
:- qb_alias(vcard, 'http://www.w3.org/2006/vcard/ns#').

:- meta_predicate
    debug_cbp(:, +, +).

:- debug(cbp).





cbp_script :-
  maplist(file_name, [cbp,cbp2bag,bag], [F3a,F3b,F3c]),
  (   maplist(exists_file, [F3a,F3b,F3b])
  ->  maplist(load_data, [F3a,F3b,F3c], [cbp,cbp2bag,bag])
  ;   file_name(cbp2, F2),
      (   exists_file(F2)
      ->  load_data(F2, cbp)
      ;   file_name(cbp1, F1),
          (   exists_file(F1)
          ->  load_data(F1, cbp)
          ;   debug_cbp(convert_cbp, loading, cbp),
              save_data(F1, cbp)
          ),
          debug_cbp(structure_postcodes, structuring, cbp),
          save_data(F2, cbp)
      )%,
      %debug_cbp(enrich_with_bag, enriching, [cbp,cbp2bag,bag]),
      %maplist(save_data, [F3a,F3b,F3c], [cbp,cbp2bag,bag])
  ).



debug_cbp(Goal_n, V, Gs) :-
  debug_cbp_start(V),
  apply(Goal_n, Gs),
  debug_cbp_end(V, Gs).



file_name(Base, File) :-
  atomic_concat(Base, '.nt.gz', LocalName),
  absolute_file_name(data(LocalName), File, [access(write)]).



load_data(File, G) :-
  access_file(File, read),
  rdf_load_file(File, [graph(G)]).



save_data(File, G) :-
  access_file(File, write),
  rdf_save_to_file(File, [compress(gzip),format(ntriples),graph(G)]).



debug_cbp_end(V, Gs) :-
  debug(cbp, "Ended ~a CBP data.", [V]),
  maplist(debug_cbp_end, Gs).



debug_cbp_end(G) :-
  rdf_statistics(triples_by_graph(G,T)),
  debug(cbp, "  - Graph ~a contains ~D triples.", [G,T]).



debug_cbp_start(V) :-
  debug(cbp, "Started ~a CBP data...", [V]).
