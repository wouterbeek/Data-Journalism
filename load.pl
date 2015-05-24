% Load file for Data Journalism.

:- dynamic(user:project/2).
:- multifile(user:project/2).
user:project(dj, "Data Journalism").


:- use_module(load_project).
:- load_project([
  plc-'Prolog-Library-Collection',
  plGraph,
  plHttp,
  plLangTag,
  plRdf,
  plSet,
  plTree,
  plUri,
  plXml,
  plXsd
]).


:- use_module(dj(cbp)).
:- use_module(dj(test)).
