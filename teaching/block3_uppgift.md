# Block 3 – Uppgift: Regressionsanalys

## Syfte

Att träna dig i att själv välja, genomföra och redovisa en regressionsanalys som svarar på en
forskningsfråga. Du ska kunna tolka resultatet statistiskt och presentera det i APA-korrekt form.

## Att göra

### 1. Välj dataset och formulera forskningsfråga

- Välj ett av kursens dataset från poolen (se dokumentationen i mappen **Data till kvantkurs**)
- Läs dokumentationen för datasettet så du förstår vilka variabler som finns
- Formulera en forskningsfråga som kan besvaras med regression:
  - En utfallsvariabel (Y) som är kontinuerlig
  - Flera möjliga prediktorer (X) som hänger ihop teoretiskt med Y
- Rita en enkel modell (på papper eller i PowerPoint): pilar från X till Y

### 2. Kör en enkel linjär regression

- En prediktor → utfallet
- Tolka lutningen (hur mycket förändras Y per enhetsökning i X?)
- Tolka R² (hur stor andel av Y förklarar prediktorn?)

### 3. Kör en multipel linjär regression

- Flera prediktorer → utfallet
- Tolka de unika bidragen (vilka prediktorer är signifikanta när du kontrollerar för de andra?)
- Jämför prediktorernas relativa vikt med **standardiserade koefficienter (β)**
- Gör en **VIF-check** för multikollinearitet och kommentera kort

### 4. Välj minst en fördjupning

Välj den/de fördjupning(ar) som passar ditt dataset och din forskningsfråga:

- **a) Moderering / interaktion** – testa om en prediktors effekt beror på en annan
- **b) Kurvilinjäritet** – testa om sambandet böjer sig (kvadratisk term)
- **c) Hierarkisk modelljämförelse** – lägg till prediktorer i steg, rapportera ΔR²
- **d) Dummykodning av kategorisk prediktor** – om ditt dataset har en nominal variabel

**Obs:** fördjupningen ska väljas utifrån vad som är meningsfullt för just ditt dataset och din
forskningsfråga. Alla fördjupningar passar inte alla dataset.

### 5. Presentera resultatet

- **APA-korrekt tabell** över den multipla regressionen (koefficienter, SE, p-värden, R²)
- **Kort resultattext** i APA-format (2–3 meningar), t.ex.:
  > *"Empowering leadership och autonom motivation predicerade arbetstillfredsställelse tillsammans, R² = .36, F(3, 282) = 52.8, p < .001. Empowering leadership (β = .42, p < .001) och autonom motivation (β = .28, p < .001) var signifikanta oberoende prediktorer, medan mission command (β = .09, p = .102) inte bidrog unikt."*
- **Minst en visualisering** (scatterplot för enkel regression, forest plot för multipel, eller annan lämplig graf)

### 6. Muntlig redovisning

- Kort presentation med stöd av PowerPoint (1–2 textbilder + 1–2 figurer)
- Beskriv: forskningsfråga → modell → resultat → tolkning
- Reflektera kort över fördjupningen du valde: vad tillförde den?

## Vad du tar med dig

Efter uppgiften ska du kunna:

- Matcha en forskningsfråga med en regressionsanalys
- Bygga enkla och multipla modeller i R
- Tolka output (koefficienter, p-värden, R², VIF)
- Skriva ihop resultaten i en APA-korrekt tabell och kort text
- Göra en fördjupning som passar ditt material
- Presentera muntligt med stöd av figur

## Hjälpkod

Se filen **`block3_helper.R`** för ett kommenterat R-skript med hjälpfunktioner. Det innehåller
färdig kod du kan modifiera med dina variabler.

## Att lämna in

- PowerPoint-presentation (kort) för muntlig redovisning
- (Valfritt) R-skript med din analys, så du har det för framtida referens
