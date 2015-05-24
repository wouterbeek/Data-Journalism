Data Journalism
===============

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
