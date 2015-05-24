Data Journalism
===============

Conversie van CBP gegevens van JSON naar RDF
--------------------------------------------

  1.  Compile the latest version of **SWI-Prolog**:
      ```bash
      $ git clone https://github.com/SWI-Prolog/swipl-devel.git
      $ cd swipl-devel
      $ ./build
      ```
  
  2.  Download the latest version of **Data-Journalism**:
      ```bash
      $ git clone https://github.com/wouterbeek/Data-Journalism.git
      $ cd Data-Journalism
      $ git submodule update --init
      ```
      
  3.  Place the data file in location `.../Data-Journalism/data/cbp.tar.gz`.
  
  4.  Run the conversion script with
      ```bash
      $ swipl run.pl
      ```
  
  Het Linked Data bestand is nu te vinden onder
  `.../Data-Journalism/data/cbp.rdf`.

Inladen in ClioPatria
---------------------

  1.  Download and configure the latest version of **ClioPatria**:
      ```bash
      $ git clone https://github.com/ClioPatria/ClioPatria.git
      $ cd ClioPatria
      $ ./configure
      ```
  
  2.  Load the data into **ClioPatria**:
      $ ./run
      ?- rdf_load('data/cbp.rdf').
      ```


Remarks about the conversion
----------------------------

Ik documenteer nog even wat details van wat ik tijdens de data conversie ben tegengekomen:

  * De data bestaat --- ontologisch gezien --- uit twee klassen: meldingen en agenten. Agenten kunnen melders, betrokkenen, verantwoordelijken, of ontvangers zijn. De subklassen waartoe een agent behoord worden geinduceerd door in de data aanwezige relaties (zie mijn volgende punt).

  * Er zijn drie soorten relaties in de data: (1) eigenschappen van een melding, (2) eigenschappen van een agent, (3) relaties tussen meldingen en agenten.

  * De volgende eigenschappen zijn optioneel:
    * Eigenschap `doorgifte_buiten_eu` van een melding.
    * De `betrokkenen` bij een melding.
    * De `ontvangers` van een melding.
    * Het `Postadres` van een verantwoordelijke agent.

  * Tussen de relatie `meldingen` en de eigenschappen van een melding zit een relatie die als naam de key van de melding heeft. Ik beschouw dit als een artefact dat niet in de conversie hoeft te worden meegenomen.

  * De naam/waarde paren in `betrokkenen` lijken vrije tekst / ongestructureerd te zijn. Het wordt lastig om hier herbruikbare entiteiten/resources van te maken.
