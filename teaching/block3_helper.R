# =============================================================================
# Block 3 — Hjälpskript för regressionsuppgiften
# =============================================================================
#
# Så här använder du skriptet:
#   1. Öppna det här skriptet i RStudio
#   2. Läs in ditt dataset (t.ex. med File > Import dataset; 
#      för enkelhetens skull så du slipper ändra i skriptet sen, döp den data frame du läser in data i till "d")
#   3. Kör ett steg i taget, modifiera variabelnamn där det behövs
#   4. Där det står ÄNDRA HÄR behöver du anpassa till ditt dataset
#
# Skriptets struktur följer uppgiftens steg.
# Extra hjälpfunktioner (deskriptiv statistik, korrelationer m.m.)
# finns längst ner i skriptet.
#
# =============================================================================


# =============================================================================
# FÖRBEREDELSER OCH DATAINLÄSNING
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Förberedelser
# -----------------------------------------------------------------------------

# Rensa arbetsmiljön (valfritt men ger en ren start)
# ta bort # framför raden för att aktivera den/göra körbar
# rm(list = ls()); graphics.off(); cat("\014")

# Kolla vilka extra paket som behövs och installera dem du saknar
nodvandiga_paket <- c("tidyverse", "broom", "psych", "car", "lm.beta")
saknade <- nodvandiga_paket[!nodvandiga_paket %in% installed.packages()[, "Package"]]
if (length(saknade) > 0) install.packages(saknade)

# Ladda paket
library(tidyverse)      # datahantering och ggplot
library(broom)          # strukturerad output från modeller (tidy, glance)
library(psych)          # deskriptiv statistik
library(car)            # för vif()
library(lm.beta)        # standardiserade betas direkt från lm-modell

# Färgpalett (FHS)
FHSpalette <- c("#00465a", "#296C7F", "#EAF0F2",
                "#481242", "#9f4494", "#f5d5ee",
                "#584d29", "#d9cea7", "#fcf6e1")
fhs_primary <- "#00465a"
fhs_accent  <- "#9f4494"


# -----------------------------------------------------------------------------
# 2. Ladda in data och inspektera
# -----------------------------------------------------------------------------

# Läs in ditt dataset (t.ex. med File > Import dataset; 
#      för enkelhetens skull så du slipper ändra i skriptet sen, döp den data frame du läser in data i till "d")
#  

# Du kan annars lägga ditt dataset i ditt working directory
# Kolla vilken mapp R arbetar i — här ska ditt dataset ligga
getwd()

# Om datasettet ligger någon annanstans, så kan du ändra arbetsmapp (var R tittar och sparar filer):
# Session → Set Working Directory → To Source File Location
# eller: 
# setwd("/din/sökväg")

setwd("/Users/gisela/Library/CloudStorage/OneDrive-Försvarshögskolan/Kurser/Kvantmetodik ForskU med Erik B/Regression")

# Lista filer i arbetsmappen (hittar du din .csv-fil här?)
list.files()

# Läs in data — ÄNDRA HÄR: byt filnamn till ditt valda dataset
d <- read.csv("Mission command_data.csv")

### CHECKPOINT: Vid denna punkt i skriptet ska du på något sätt ha läst in en csv-fil till en data frame som heter "d"

# Titta på datan
dim(d)               # antal rader och kolumner
names(d)             # alla variabelnamn
View(d)              # öppnar datan för inspektion


# -----------------------------------------------------------------------------
# 3. Hitta dina variabler (och ev. skapa index från items)
# -----------------------------------------------------------------------------
# Det är lätt att hitta alla items som börjar med ett visst prefix:

# ÄNDRA HÄR: byt "MC_" mot prefixet för den skala du vill skapa index för
# Hatten i början: ^MC_ betyder att den ska hämta variabelnamn som BÖRJAR med MC_ alltså viktigt att du behåller hatten
mina_items <- grep("^MC_", names(d), value = TRUE)
mina_items

# Skapa index som medelvärde över items (hoppa över om ditt dataset redan
# har färdiga index, kanske som du skapat tidigare
d$mission_command <- rowMeans(d[, mina_items], na.rm = TRUE)

# Upprepa för dina andra skalor — ÄNDRA HÄR för varje skala
# mitt_index_2 <- grep("^EL_", names(d), value = TRUE)
# d$empowering_leadership <- rowMeans(d[, mitt_index_2], na.rm = TRUE)


# =============================================================================
# UPPGIFTENS STEG 2: Enkel linjär regression
# =============================================================================

# -----------------------------------------------------------------------------
# 4. Kör enkel regression
# -----------------------------------------------------------------------------
# ÄNDRA HÄR: byt Y och X mot dina variabler

modell_enkel <- lm(Y ~ X, data = d)
summary(modell_enkel)

# Visualisera sambandet med scatterplot och regressionslinje
# alpha = 0.5 gör punkterna halvgenomskinliga (bra när många punkter överlappar)
ggplot(d, aes(x = X, y = Y)) +                    # ÄNDRA HÄR
  geom_point(alpha = 0.5, color = fhs_primary) +
  geom_smooth(method = "lm", se = TRUE, color = fhs_accent) +
  labs(title = "Enkel regression: X → Y",         # ÄNDRA HÄR
       x = "Prediktor",
       y = "Utfall") +
  theme_minimal(base_size = 13)


# =============================================================================
# UPPGIFTENS STEG 3: Multipel linjär regression
# =============================================================================

# -----------------------------------------------------------------------------
# 5. Kör multipel regression
# -----------------------------------------------------------------------------

# ÄNDRA HÄR: byt Y och prediktorerna mot dina variabler

modell_multipel <- lm(Y ~ X1 + X2 + X3, data = d)
summary(modell_multipel)

# Strukturerad output med konfidensintervall — kan vara lättare att läsa
tidy(modell_multipel, conf.int = TRUE)


# -----------------------------------------------------------------------------
# 6. Standardiserade koefficienter (β)
# -----------------------------------------------------------------------------
# För att jämföra prediktorers relativa vikt använder vi standardiserade
# koefficienter (β). Paketet lm.beta räknar ut dem direkt från din lm-modell
# — du behöver inte standardisera variablerna först.
#
# OBS: lm.beta() tar en redan kör lm-modell som argument (inte formel + data).

modell_std <- lm.beta(modell_multipel)
summary(modell_std)   # visar både B (ostandardiserade) och β (standardiserade)


# -----------------------------------------------------------------------------
# 7. VIF-check (multikollinearitet)
# -----------------------------------------------------------------------------
# VIF (Variance Inflation Factor) är ett vanligt förekommande mått
# som du kommer se rapporteras i andra artiklar — men det är lite omdiskuterat.
#
# Vanliga tumregler:
#   VIF ≈ 1   — prediktorn är i princip oberoende av de andra
#   VIF < 2   — strängt riktmärke
#   VIF < 5   — vanlig acceptansgräns (James et al., 2013)
#
#  Nyare metodlitteratur är kritisk till att använda VIF-tröskelvärden
# för att avfärda multikollinearitetsproblem:
#   - Kalnins & Praitis Hill (2024) — låga VIF kan samexistera med bias;
#     bättre att titta på bivariata korrelationer (|r| > .30 som varning)
#
# Rapportera VIF, men titta också på din korrelationstabell
# (se "Extra hjälpfunktioner" i slutet av skriptet).

vif(modell_multipel)



# -----------------------------------------------------------------------------
# 8. Visualisering: forest plot av standardiserade betas
# -----------------------------------------------------------------------------
# Bra om du vill jämföra prediktorernas relativa vikt visuellt.

# Hämta data från lm.beta-objektet
coef_tab <- summary(modell_std)$coefficients[-1, ]   # hoppa över intercept
betas   <- coef_tab[, "Standardized"]
t_vals  <- coef_tab[, "t value"]
pvals   <- coef_tab[, "Pr(>|t|)"]
ses_std <- betas / t_vals   # standardiserad SE = β / t

forest_df <- data.frame(
  Prediktor   = rownames(coef_tab),
  Beta        = betas,
  CI_lo       = betas - 1.96 * ses_std,
  CI_hi       = betas + 1.96 * ses_std,
  Signifikans = ifelse(pvals < 0.05, "Signifikant", "Ej sig.")
)

ggplot(forest_df, aes(x = Beta, y = reorder(Prediktor, Beta),
                       color = Signifikans)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = CI_lo, xmax = CI_hi),
                 height = 0.2, linewidth = 0.8) +
  geom_point(size = 4) +
  scale_color_manual(values = c("Signifikant" = fhs_primary,
                                 "Ej sig."    = "#999999")) +
  labs(title = "Standardiserade regressionskoefficienter",
       x = "β (med 95% KI)", y = NULL) +
  theme_minimal(base_size = 13)


# =============================================================================
# UPPGIFTENS STEG 4: Fördjupning — välj minst en
# =============================================================================

# -----------------------------------------------------------------------------
# 9a. Moderering / interaktion
# -----------------------------------------------------------------------------
# Testa om effekten av X1 beror på X2 (t.ex. "buffrar X2 X1?")

# Centrera prediktorerna för tolkbar interaktion
d$X1_c <- d$X1 - mean(d$X1, na.rm = TRUE)         # ÄNDRA HÄR
d$X2_c <- d$X2 - mean(d$X2, na.rm = TRUE)         # ÄNDRA HÄR

modell_int <- lm(Y ~ X1 + X2 + X3 + X1_c:X2_c, data = d)   # ÄNDRA HÄR
summary(modell_int)


# -----------------------------------------------------------------------------
# 9b. Kurvilinjäritet
# -----------------------------------------------------------------------------
# Lägg till en kvadratisk term för att testa om sambandet böjer sig
# (t.ex. omvänd U-form med optimum i mitten)

modell_quad <- lm(Y ~ X1 + I(X1^2), data = d)      # ÄNDRA HÄR
summary(modell_quad)

# Hitta optimum ifall den kvadratiska termen är signifikant:
# -b1 / (2*b2)
b1 <- coef(modell_quad)[2]
b2 <- coef(modell_quad)[3]
optimum <- -b1 / (2 * b2)
cat("Estimerat optimum för X1:", round(optimum, 2), "\n")

# Du kan ta med din kvadrerade prediktor i den multipla regressionen ovanför också.


# -----------------------------------------------------------------------------
# 9c. Hierarkisk modelljämförelse
# -----------------------------------------------------------------------------
# Lägg till prediktorer steg för steg och testa ΔR²
# Typ: Steg 1 = kontrollvariabler, Steg 2 = teoretiska prediktorer

modell_steg1 <- lm(Y ~ kontrollvariabel, data = d)             # ÄNDRA HÄR
modell_steg2 <- lm(Y ~ kontrollvariabel + X1 + X2, data = d)   # ÄNDRA HÄR

# Formellt F-test för ΔR²
anova(modell_steg1, modell_steg2)

# R² för varje steg
summary(modell_steg1)$r.squared
summary(modell_steg2)$r.squared
# ΔR²
summary(modell_steg2)$r.squared - summary(modell_steg1)$r.squared


# -----------------------------------------------------------------------------
# 9d. Dummykodning av kategorisk prediktor
# -----------------------------------------------------------------------------
# Vissa variabler är NUMERISKT kodade (1, 2, 3...) men det är inte alltid
# självklart att de ska behandlas som kontinuerliga i en regression.
#
# Tre möjliga tillvägagångssätt:
#   a) Numerisk (1, 2, 3) — antar både ORDNING och LIKA AVSTÅND i effekt
#   b) Ordinal factor       — antar bara ordning
#   c) Dummykodad factor    — antar bara olika kategorier (ingen ordning)
#
# Exempel: I ambush-datan är 'priority' kodad 1, 2, 3 och står för
# tre strategier (skydd / avvägning / uppdrag). Det finns en logisk
# ordning, men är "avvägning" verkligen exakt mittemellan de andra i effekt?
# Testa båda och jämför.
#
# R dummykodar automatiskt om du gör variabeln till en factor:
# - Med K kategorier skapas K-1 dummyvariabler internt
# - Första nivån blir REFERENS (ingår i intercept)
# - Övriga nivåer får var sin koefficient (skillnad mot referens)

# Som numerisk (standardkodningen)
modell_numerisk <- lm(Y ~ X1 + priority, data = d)       # ÄNDRA HÄR

# Som dummykodad factor — R sköter dummykodningen automatiskt
d$priority_cat <- factor(d$priority)                     # ÄNDRA HÄR
modell_dummy <- lm(Y ~ X1 + priority_cat, data = d)      # ÄNDRA HÄR

# Kolla vilken kategori som är referens (den första i listan)
levels(d$priority_cat)

# Jämför resultaten
summary(modell_numerisk)
summary(modell_dummy)

# I outputen för dummykodad:
#   (Intercept)     = Ŷ när priority_cat = referenskategorin
#   priority_cat2   = skillnad mellan kategori 2 och referensen
#   priority_cat3   = skillnad mellan kategori 3 och referensen
#
# Tolkning:
# - Om dummy-koefficienterna ligger proportionellt (t.ex. 2 → +0.15, 3 → +0.30)
#   → antagandet om lika avstånd i den numeriska modellen stämmer rimligt
# - Om de är "udda" (t.ex. 2 → +0.05, 3 → +0.40)
#   → numerisk kodning är missvisande — behåll dummykodningen


# =============================================================================
# EXTRA HJÄLPFUNKTIONER (kör vid behov)
# =============================================================================

# -----------------------------------------------------------------------------
# Deskriptiv statistik
# -----------------------------------------------------------------------------
# M, SD, min, max m.m. Bra att ha till din APA-rapport.
# ÄNDRA HÄR: byt ut variabelnamnen mot dina egna

psych::describe(d[, c("Y", "X1", "X2", "X3")])


# -----------------------------------------------------------------------------
# Bivariat korrelationstabell
# -----------------------------------------------------------------------------
# Hur hänger variablerna ihop parvis? Bra komplement till regressionen.
# ÄNDRA HÄR: byt ut variabelnamnen mot dina egna

cor(d[, c("Y", "X1", "X2", "X3")], use = "complete.obs")

# För p-värden för korrelationerna:
psych::corr.test(d[, c("Y", "X1", "X2", "X3")])


# -----------------------------------------------------------------------------
# Exportera figur för PowerPoint
# -----------------------------------------------------------------------------
# Spara senaste figuren som PNG

ggsave("min_figur.png", width = 8, height = 5, dpi = 300)


# =============================================================================
# KLART!
# =============================================================================
