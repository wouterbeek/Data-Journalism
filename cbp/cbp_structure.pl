:- module(
  cbp_structure,
  [
    structure_postcodes/1 % +CbpG
  ]
).

/** <module> Add structure to CBP data

@author Wouter Beek
@version 2015/05/29, 2016/05
*/

:- use_module(library(atom_ext).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(q/qb)).
:- use_module(library(semweb/rdf11)).





%! structure_postcodes(+M, +G) is det.
% Adds more structure to CBP postcodes.
%
% For example [1] is converted to [2]:
%
% ```turtle
% [1]   Zutphenseweg 51\n7418AH DEVENTER\nNederland
% [2]   _:1 a                   vcard:Address ;
%           vcard:streetAddress "Zutphenseweg 51"^^xsd:string ;
%           vcard:locality      "Deventer"^^xsd:string ;
%           vcard:postal-code   "7418AH"^^xsd:string .
% ```

structure_postcodes(M, G) :-
  flag(number_of_structured_postcodes, _, 0), %DEB
  q(M, Agent, cbpo:bezoekAdres, String^^xsd:string, G), %NONDET
  rdf_retractall(Agent, cbpo:bezoekAdres, _, G),
  create_postcode(M, Agent, String, G),
  flag(number_of_structured_postcodes, N, N + 1), %DEB
  debug(cbp, "Structured ~D postcodes.", [N]), %DEB
  fail.
structure_postcodes(_, _).



%! create_postcode(+M, +Agent, +String, +G) is det.

create_postcode(M, Agent, String, G) :-
  once(atom_phrase(postcode(StreetAddress, Locality, PostalCode), String)),
  qq_iri(cbpr, Address),
  qb_instance(M, Address, vcard:'Address', G),
  qb(M, Agent, cbpo:bezoekAdres, Address, G),
  qb(M, Address, vcard:streetAddress, StreetAddress^^xsd:string, G),
  qb(M, Address, vcard:locality, Locality^^xsd:string, G),
  qb(M, Address, vcard:'postal-code', PostalCode^^xsd:string, G).



%! postcode(-StreetAddress, -Locality, -PostalCode)// is det.

postcode(StreetAddress, Locality, PostalCode) -->
  % Street address.
  '...'(Cs1), "\n", !,
  {atom_codes(StreetAddress, Cs1)},

  % Postal code.
  '...'(Cs2), " ", !,
  {atom_codes(PostalCode, Cs2)},

  % Locality.
  '...'(Cs3), "\n", !,
  {
    atom_codes(A1, Cs3),
    downcase_atom(A1, A2),
    capitalize_atom(A2, Locality)
  },

  % Rest.
  '...'.
