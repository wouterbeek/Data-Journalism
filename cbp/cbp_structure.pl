:- module(
  cbp_structure,
  [
    structure_postcodes/1 % +CbpGraph:atom
  ]
).

/** <module> Add structure to CBP data

@author Wouter Beek
@version 2015/05/29
*/

:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plc(generics/atom_ext)).
:- use_module(plc(dcg/dcg_content)).
:- use_module(plc(dcg/dcg_generics)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdf_read)).

%! structure_postcodes(+Graph:atom) is det.
% Adds more structure to CBP postcodes.
%
% For example [1] is converted to [2]:
%
% ```
% [1]   Zutphenseweg 51\n7418AH DEVENTER\nNederland
% [2]   _:1 a                   vcard:Address ;
%           vcard:streetAddress "Zutphenseweg 51"^^xsd:string ;
%           vcard:locality      "Deventer"^^xsd:string ;
%           vcard:postal-code   "7418AH"^^xsd:string .
% ```

structure_postcodes(G):-
  flag(number_of_structured_postcodes, _, 0), %DEB
  rdf_string(Agent, cbpo:bezoekAdres, String, G), %NONDET
  rdf_retractall(Agent, cbpo:bezoekAdres, _, G),
  create_postcode(Agent, String, G),
  flag(number_of_structured_postcodes, N, N + 1), %DEB
  debug(cbp, 'Structured ~D postcodes.', [N]), %DEB
  fail.
structure_postcodes(_).

%! create_postcode(+Agent:iri, +String:atom, +Graph:atom) is det.

create_postcode(Agent, String, G):-
  once(atom_phrase(postcode(StreetAddress, Locality, PostalCode), String)),
  rdf_create_next_resource(cbpr, [], vcard:'Address', G, Address),
  rdf_assert(Agent, cbpo:bezoekAdres, Address, G),
  rdf_assert_string(Address, vcard:streetAddress, StreetAddress, G),
  rdf_assert_string(Address, vcard:locality, Locality, G),
  rdf_assert_string(Address, vcard:'postal-code', PostalCode, G).

%! postcode(-StreetAddress:atom, -Locality:atom, -PostalCode:atom)// is det.

postcode(StreetAddress, Locality, PostalCode) -->
  % Street address.
  '...'(Cs1), "\n", !,
  {atom_codes(StreetAddress, Cs1)},

  % Postal code.
  '...'(Cs2), " ", !,
  {atom_codes(PostalCode, Cs2)},

  % Locality.
  '...'(Cs3), "\n", !,
  {atom_codes(A1, Cs3), downcase_atom(A1, A2), capitalize_atom(A2, Locality)},

  % Rest.
  '...'.
