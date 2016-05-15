:- module(
  test,
  [
    print_headers/0,
    print_streams/0
  ]
).

/*
Inladen in ClioPatria
---------------------

  1.  Download en configureer de laatste versie van **ClioPatria**:
      ```bash
      $ git clone https://github.com/ClioPatria/ClioPatria.git
      $ cd ClioPatria
      $ ./configure
      ```
  
  2.  Laad de data in **ClioPatria**:
      ```bash
      $ ./run
      ?- rdf_load('data/cbp.rdf').
      ```
*/

:- use_module(library(dict_ext)).
:- use_module(library(http/http_download)).
:- use_module(library(os/archive_ext)).





print_headers :-
  absolute_file_name('data/cbp.tar.gz', File, [access(read)]),
  archive_info(File).



print_streams :-
  absolute_file_name('data/cbp.tar.gz', File, [access(read)]),
  forall(json_download(File, Json), print_dict(Json)).
