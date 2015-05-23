% Debug startup file for Data Journalism.


:- use_module(library(debug)).
:- debug(lodCache).


:- [load].


:- use_module(debug_project).
%:- debug_all_files.


% Make available from the top-level.
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(semweb/rdfs)).
