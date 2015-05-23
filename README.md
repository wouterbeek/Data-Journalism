Data Journalism
===============

Conversie van CBP gegevens van JSON naar RDF
--------------------------------------------

Ik documenteer nog even wat details van wat ik tijdens de data conversie ben tegengekomen:

  * De data bestaat --- ontologisch gezien --- uit twee klassen: meldingen en agenten. Agenten kunnen melders, betrokkenen, verantwoordelijken, of ontvangers zijn. De subklassen waartoe een agent behoord worden geinduceerd door bijbehorende relaties (zie mijn volgende punt).

  * Er zijn drie soorten relaties in de data: (1) eigenschappen van een melding, (2) eigenschappen van een agent, (3) relaties tussen meldingen en agenten.

  * De volgende eigenschappen zijn optioneel:
    * Eigenschap `doorgifte_buiten_eu` van een melding.
    * De `betrokkenen` bij een melding.
    * De `ontvangers` van een melding.
    * Het `Postadres` van een verantwoordelijke agent.

  * Tussen de relatie `meldingen` en de eigenschappen van een melding zit een relatie die als naam de key van de melding heeft. Ik beschouw dit als een artefact dat niet in de conversie hoeft worden meegenomen.

  * De naam/waarde paren in `betrokkenen` lijken vrije tekst / ongestructureerd te zijn. Het wordt lastig om hier entiteiten/resources van te maken.
