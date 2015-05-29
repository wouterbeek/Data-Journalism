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

:- use_module(library(archive)).
:- use_module(library(http/json)).

:- use_module(plc(dcg/dcg_pl_term)).
:- use_module(plc(generics/code_ext)).



print_headers:-
  absolute_file_name('data/cbp.tar.gz', File, [access(read)]),
  setup_call_cleanup(
    archive_open(File, Archive, []),
    (
      repeat,
      (   archive_next_header(Archive, Entry)
      ->  format('~w~n', [Entry]),
          fail
      ;   !
      )
    ),
    archive_close(Archive)
  ).


print_streams:-
  absolute_file_name('data/cbp.tar.gz', File, [access(read)]),
  setup_call_cleanup(
    archive_open(File, Archive, []),
    (
      repeat,
      (   archive_data_stream(Archive, In, [meta_data([H|_])])
      ->  format('~w~n', [H.name]),
          call_cleanup(
            (   json_read_dict(In, Dict),
                print_dict(Dict)
            ),
            close(In)
          ),
          fail
      ;   !
      )
    ),
    archive_close(Archive)
  ).



% HELPERES %

print_dict(D):-
  phrase(dcg_pl_term(D), Cs),
  put_codes(Cs),
  nl.
