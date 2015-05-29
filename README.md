CBP data
--------

De data kan bevraagd worden middels [SPARQL](http://cliopatria.lodlaundromat.d2s.labs.vu.nl/yasgui/index.html) en [SWISH](http://cliopatria.lodlaundromat.d2s.labs.vu.nl/swish/).

CBP vocabulair
--------------

![](https://raw.githubusercontent.com/wouterbeek/Data-Journalism/master/cbpo.png)
  
Bovenstaande graaf laat het vocabulair van de CBP dataset zien.

De blokken representeren de basis entiteiten: meldingen, agenten, adressen.

Relaties tussen de basis entiteiten zijn weergegeven met gelabelde pijlen.

Eigenschappen van de basis entiteiten zijn binnen de afzonderlijke blokken weergegeven (naam van de eigenschap, gevolgd door datatype).

Optionele relaties/eigenschappen zijn aangeduid met `[OPT]`.

De volgende prefixes worden gebruikt om de namen af te korten:

| Prefix    | Expansion                                                                      |
|:--------- |:------------------------------------------------------------------------------ |
| `cbpo`    | [http://www.cbp.nl/ontology/](http://www.cbp.nl/ontology/)                     |
| `dcterms` | [http://purl.org/dc/terms/](http://purl.org/dc/terms/)                         |
| `foaf`    | [http://xmlns.com/foaf/0.1/](http://xmlns.com/foaf/0.1/)                       |
| `rdfs`    | [http://www.w3.org/2000/01/rdf-schema#](http://www.w3.org/2000/01/rdf-schema#) |
| `vcard`   | [http://www.w3.org/2006/vcard/ns#](http://www.w3.org/2006/vcard/ns#)           |
| `xsd`     | [http://www.w3.org/2001/XMLSchema#](http://www.w3.org/2001/XMLSchema#)         |



Conversie van CBP gegevens van JSON naar RDF
--------------------------------------------

  1.  Compileer de laatste versie van **SWI-Prolog**:
      ```bash
      $ git clone https://github.com/SWI-Prolog/swipl-devel.git
      $ cd swipl-devel
      $ ./build
      ```
  
  2.  Download de laatste versie van **Data-Journalism**:
      ```bash
      $ git clone https://github.com/wouterbeek/Data-Journalism.git
      $ cd Data-Journalism
      $ git submodule update --init
      ```
      
  3.  Plaats het data bestand in locatie `.../Data-Journalism/data/cbp.tar.gz`.
  
  4.  Draai het conversie script met:
      ```bash
      $ swipl run.pl
      ?- convert_cbp.
      ```
  
  Het Linked Data bestand is nu te vinden in locatie
  `.../Data-Journalism/data/cbp.nt.gz`.



Opmerkingen over de conversie
-----------------------------

  * De data bestaat --- ontologisch gezien --- uit twee klassen: meldingen en agenten. Agenten kunnen melders, betrokkenen, verantwoordelijken, of ontvangers zijn. De subklassen waartoe een agent behoord worden geinduceerd door in de data aanwezige relaties (zie mijn volgende punt).

  * Er zijn drie soorten relaties in de data: (1) eigenschappen van een melding, (2) eigenschappen van een agent, (3) relaties tussen meldingen en agenten.

  * De volgende eigenschappen zijn optioneel:
    * Eigenschap `doorgifte_buiten_eu` van een melding.
    * De `betrokkenen` bij een melding.
    * De `ontvangers` van een melding.
    * Het `Postadres` van een verantwoordelijke agent.

  * Tussen de relatie `meldingen` en de eigenschappen van een melding zit een relatie die als naam de key van de melding heeft. Ik beschouw dit als een artefact dat niet in de conversie hoeft te worden meegenomen.

  * De naam/waarde paren in `betrokkenen` lijken vrije tekst / ongestructureerd te zijn. Het wordt lastig om hier herbruikbare entiteiten/resources van te maken.
