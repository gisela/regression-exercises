# =============================================================================
# Block 3 — Hjälpskript för regressionsuppgiften
# =============================================================================
#
# Så här använder du skriptet:
#   1. Lägg ditt valda dataset (.csv) i din working directory
#   2. Öppna det här skriptet i RStudio
#   3. Kör ett steg i taget, modifiera variabelnamn där det behövs
#   4. Där det står ÄNDRA HÄR behöver du anpassa till ditt dataset
#
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Förberedelser
# -----------------------------------------------------------------------------

# Rensa arbetsmiljön (valfritt men ger en ren start)
rm(list = ls()); graphics.off(); cat("\014")

# Ladda paket (installera först om du inte har dem: install.packages("namn"))
library(tidyverse)      # datahantering och ggplot
library(broom)          # strukturerad output från modeller
library(modelsummary)   # APA-snygga regressionstabeller
library(car)            # för vif()

# Färgpalett (FHS)
FHSpalette <- c("#00465a", "#296C7F", "#EAF0F2",
                "#481242", "#9f4494", "#f5d5ee",
                "#584d29", "#d9cea7", "#fcf6e1")
fhs_primary <- "#00465a"
fhs_accent  <- "#9f4494"


# -----------------------------------------------------------------------------
# 2. Ladda in data och inspektera
# -----------------------------------------------------------------------------

# Kolla vilken mapp R arbetar i — här ska ditt dataset ligga
getwd()

# Om datasettet ligger någon annanstans, ändra arbetsmapp:
# Session → Set Working Directory → To Source File Location
# eller: setwd("/sökväg/till/din/mapp")

# Lista filer i arbetsmappen (hittar du din .csv-fil här?)
list.files()

# Läs in data — ÄNDRA HÄR: byt filnamn till ditt valda dataset
d <- read.csv("Mission command_data.csv")

# Titta på datan
dim(d)               # antal rader och kolumner
names(d)             # alla variabelnamn
View(d)              # öppnar datan i RStudio (valfritt)


# -----------------------------------------------------------------------------
# 3. Hitta och välj dina variabler
# -----------------------------------------------------------------------------

# Datasetten har ofta items med liknande prefix, t.ex. MC_01, MC_02 ...
# Det är lätt att hitta alla items som börjar med ett visst prefix:

# ÄNDRA HÄR: byt "MC_" mot prefixet för din skala
mina_items <- grep("^MC_", names(d), value = TRUE)
mina_items


# -----------------------------------------------------------------------------
# 4. Skapa index (om ditt dataset har items)
# -----------------------------------------------------------------------------
# Ett index är ett medelvärde över flera items som mäter samma sak.
# Vissa dataset har redan färdiga index — då kan du hoppa över det här steget.

# Exempel: skapa index för mission command
d$mission_command <- rowMeans(d[, mina_items], na.rm = TRUE)

# Upprepa för dina andra skalor — ÄNDRA HÄR för varje skala
# mitt_index_2 <- grep("^EL_", names(d), value = TRUE)
# d$empowering_leadership <- rowMeans(d[, mitt_index_2], na.rm = TRUE)
# ...

# Kolla att det gick rätt
summary(d$mission_command)


# -----------------------------------------------------------------------------
# 5. Enkel regression
# -----------------------------------------------------------------------------
# ÄNDRA HÄR: byt Y och X mot dina variabler

model_enkel <- lm(Y ~ X, data = d)
summary(model_enkel)

# Visualisera sambandet
ggplot(d, aes(x = X, y = Y)) +                    # ÄNDRA HÄR
  geom_point(alpha = 0.5, color = fhs_primary) +
  geom_smooth(method = "lm", se = TRUE, color = fhs_accent) +
  labs(title = "Enkel regression: X → Y",         # ÄNDRA HÄR
       x = "Prediktor",
       y = "Utfall") +
  theme_minimal(base_size = 13)


# -----------------------------------------------------------------------------
# 6. Multipel regression
# -----------------------------------------------------------------------------
# ÄNDRA HÄR: byt Y och prediktorerna mot dina variabler

model_multipel <- lm(Y ~ X1 + X2 + X3, data = d)
summary(model_multipel)

# Strukturerad output (broom)
tidy(model_multipel, conf.int = TRUE)


# -----------------------------------------------------------------------------
# 7. Standardiserade koefficienter (β)
# -----------------------------------------------------------------------------
# För att jämföra prediktorers relativa vikt, standardisera alla variabler

d_z <- d
# ÄNDRA HÄR: lägg till alla dina variabler i listan
vars_to_std <- c("Y", "X1", "X2", "X3")
for (v in vars_to_std) {
  d_z[[v]] <- as.numeric(scale(d_z[[v]]))
}

model_std <- lm(Y ~ X1 + X2 + X3, data = d_z)     # ÄNDRA HÄR
round(coef(model_std)[-1], 3)   # standardiserade betas


# -----------------------------------------------------------------------------
# 8. VIF-check (multikollinearitet)
# -----------------------------------------------------------------------------
# VIF < 5 = inget problem
# VIF 5-10 = kan vara problematiskt
# VIF > 10 = allvarlig multikollinearitet

vif(model_multipel)


# -----------------------------------------------------------------------------
# 9. APA-korrekt regressionstabell
# -----------------------------------------------------------------------------
# modelsummary ger en snygg HTML-tabell du kan klistra in i PowerPoint

modelsummary(
  list("Enkel modell" = model_enkel,
       "Multipel modell" = model_multipel),
  stars = TRUE,                                  # *, **, ***
  statistic = "({std.error})",
  gof_map = c("nobs", "r.squared", "adj.r.squared", "F")
)


# -----------------------------------------------------------------------------
# 10. Visualisering: forest plot av standardiserade betas
# -----------------------------------------------------------------------------

# Hämta koefficienter med konfidensintervall
betas <- coef(model_std)[-1]
ses   <- summary(model_std)$coefficients[-1, "Std. Error"]
pvals <- summary(model_std)$coefficients[-1, "Pr(>|t|)"]

forest_df <- data.frame(
  Prediktor   = names(betas),
  Beta        = betas,
  CI_lo       = betas - 1.96 * ses,
  CI_hi       = betas + 1.96 * ses,
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


# -----------------------------------------------------------------------------
# 11. Fördjupning — välj en (eller flera) som passar ditt dataset
# -----------------------------------------------------------------------------

# ---- a) Moderering / interaktion --------------------------------------------
# Testa om effekten av X1 beror på X2

# Centrera prediktorerna för tolkbar interaktion
d$X1_c <- d$X1 - mean(d$X1, na.rm = TRUE)         # ÄNDRA HÄR
d$X2_c <- d$X2 - mean(d$X2, na.rm = TRUE)         # ÄNDRA HÄR

model_int <- lm(Y ~ X1 + X2 + X3 + X1_c:X2_c, data = d)   # ÄNDRA HÄR
summary(model_int)


# ---- b) Kurvilinjäritet -----------------------------------------------------
# Lägg till en kvadratisk term för att testa om sambandet böjer sig

model_quad <- lm(Y ~ X1 + I(X1^2), data = d)      # ÄNDRA HÄR
summary(model_quad)

# Hitta optimum om den kvadratiska termen är signifikant:
# -b1 / (2*b2)
b1 <- coef(model_quad)[2]
b2 <- coef(model_quad)[3]
optimum <- -b1 / (2 * b2)
cat("Estimerat optimum för X1:", round(optimum, 2), "\n")


# ---- c) Hierarkisk modelljämförelse -----------------------------------------
# Lägg till prediktorer i steg, testa ΔR²

model_steg1 <- lm(Y ~ kontrollvariabel, data = d)             # ÄNDRA HÄR
model_steg2 <- lm(Y ~ kontrollvariabel + X1 + X2, data = d)   # ÄNDRA HÄR

# Formellt F-test för ΔR²
anova(model_steg1, model_steg2)

# R² för varje steg
summary(model_steg1)$r.squared
summary(model_steg2)$r.squared
# ΔR²
summary(model_steg2)$r.squared - summary(model_steg1)$r.squared


# ---- d) Dummykodning av kategorisk prediktor --------------------------------
# Gör R till att behandla en variabel som kategorisk

# Gör variabeln till factor (R dummykodar automatiskt i regressionen)
d$min_kategorisk <- factor(d$min_kategorisk)             # ÄNDRA HÄR

# Nu kan du använda den i en regression:
model_dummy <- lm(Y ~ X1 + min_kategorisk, data = d)     # ÄNDRA HÄR
summary(model_dummy)

# Kolla vilken kategori som är referens
levels(d$min_kategorisk)


# -----------------------------------------------------------------------------
# 12. Exportera figur för PowerPoint
# -----------------------------------------------------------------------------

# Spara senaste figuren som PNG
ggsave("min_figur.png", width = 8, height = 5, dpi = 300)


# =============================================================================
# KLART! Du har nu:
#   - Enkel regression
#   - Multipel regression
#   - Standardiserade betas
#   - VIF-check
#   - APA-tabell
#   - Visualisering
#   - (valfri) fördjupning
# =============================================================================
