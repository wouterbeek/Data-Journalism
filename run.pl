% Standalone startup for Data Journalism.

:- if(current_prolog_flag(argv, ['--debug'])).
  :- ensure_loaded(debug).
:- else.
  :- set_prolog_flag(verbose, silent).
  :- ensure_loaded(load).
:- endif.
