Preliminaries
=============

Prefix notatie
--------------

```sparql
PREFIX cbpo: <http://www.cbp.nl/ontology/>
PREFIX cbpr: <http://www.cbp.nl/resource/>
PREFIX dcterms: <http://purl.org/dc/terms/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
```



Concrete vragen (SPARQL)
========================

Toon meldingen met hun verantwoordelijken
-----------------------------------------

```sparql
PREFIX cbpo: <http://www.cbp.nl/ontology/>
PREFIX dcterms: <http://purl.org/dc/terms/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?ml ?al
WHERE {
  ?m cbpo:verantwoordelijke ?a ;
     dcterms:description ?ml .
  ?a rdfs:label ?al
}
LIMIT 10
```



Wie is verantwoordelijk voor de meeste meldingen?
-------------------------------------------------

```sparql
PREFIX cbpo: <http://www.cbp.nl/ontology/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?vl (COUNT(?m) AS ?mn)
WHERE {
  ?m cbpo:verantwoordelijke ?v .
  ?v rdfs:label ?vl
}
GROUP BY ?vl
```

Antwoord:
  1. "SIOD"@nl	"98"^^xsd:integer
  2. "De korpsbeheerder van de politieregio Brabant Zuid-Oost"@nl	"95"^^xsd:integer
  3. "Raad van Bestuur Alysis Zorggroep"@nl	"92"^^xsd:integer
  4. "Cabot Corporation"@nl	"91"^^xsd:integer
  5. "Minister van Buitenlandse Zaken"@nl	"91"^^xsd:integer
  6. "ABN AMRO Bank N.V."@nl	"9"^^xsd:integer
  7. "AH BOER, DE"@nl	"9"^^xsd:integer
  8. "AIG Europe Limited"@nl	"9"^^xsd:integer
  9. "AWH van den Groenendaal"@nl	"9"^^xsd:integer
  10. "Albumprinter Productions B.V."@nl	"9"^^xsd:integer


Concrete vragen (natuurlijke taal)
==================================

  * welke instanties hebben toegang tot naw of medische gegevens?
  * welk bedrijf meldt zijn it monitoring?
  * wie deelt gegevens naar derden?
  * van de top 20 overheid instanties, welke gegevens hebben ze?
  * welke overheid instanties missen er in de set?
