# ==============================================================================
# 05_etape3_transformation.R
# Étape 3 – Transformation et valeur ajoutée du mil (EDA uniquement)
# ==============================================================================

source(here::here("scripts", "00_setup.R"))

create_dir(here::here("outputs", "eda"))

cat("EDA – Étape 3 : Transformation et valeur ajoutée du mil\n\n")

# ------------------------------------------------------------------------------
# 1. Chargement des données brutes
# ------------------------------------------------------------------------------

s00_raw  <- load_dta(paths$raw_menage, "s00_me_mli2021.dta")
s01_raw  <- load_dta(paths$raw_menage, "s01_me_mli2021.dta")
s07b_raw <- load_dta(paths$raw_menage, "s07b_me_mli2021.dta")
s09b_raw <- load_dta(paths$raw_menage, "s09b_me_mli2021.dta")
s10a_raw <- load_dta(paths$raw_menage, "s10a_me_mli2021.dta")
s10b_raw <- load_dta(paths$raw_menage, "s10b_me_mli2021.dta")
s16d_raw <- load_dta(paths$raw_menage, "s16d_me_mli2021.dta")
pond     <- load_dta(paths$raw_auxiliaires, "ehcvm_ponderations_mli2021.dta")

# ------------------------------------------------------------------------------
# 2. Identifiants et poids
# ------------------------------------------------------------------------------

creer_hhid <- function(df) {
  df %>% mutate(hhid = paste(grappe, menage, vague, sep = "_"))
}

s00_raw  <- creer_hhid(s00_raw)
s01_raw  <- creer_hhid(s01_raw)
s07b_raw <- creer_hhid(s07b_raw)
s09b_raw <- creer_hhid(s09b_raw)
s10a_raw <- creer_hhid(s10a_raw)
s10b_raw <- creer_hhid(s10b_raw)
s16d_raw <- creer_hhid(s16d_raw)

poids_menages <- s00_raw %>%
  select(grappe, menage, vague, hhid) %>%
  left_join(pond, by = c("grappe", "menage")) %>%
  select(hhid, hhweight)

# ==============================================================================
# EXPLORATION DES DONNÉES
# ==============================================================================

sink(here::here("outputs", "eda", "eda_etape3.txt"), split = TRUE)
cat("EDA – Étape 3 : Transformation et valeur ajoutée du mil\n")
cat("Date :", format(Sys.time()), "\n\n")

# --- Consommation de farine et semoule de mil (codes 14 et 15) ---
cat("--- 3.1 Consommation de farine et semoule de mil ---\n")

conso_farine <- s07b_raw %>%
  filter(s07bq01 %in% c(14, 15), s07bq02 == 1)

cat("Nombre de lignes de consommation (farine/semoule) :", nrow(conso_farine), "\n")

cat("Modes d'acquisition :\n")
print(table(conso_farine$s07bq06, useNA = "ifany"))

cat("\nQuantités consommées (farine/semoule) – UML :\n")
cat("  min :", min(conso_farine$s07bq03a, na.rm = TRUE),
    " / médiane :", median(conso_farine$s07bq03a, na.rm = TRUE),
    " / max :", max(conso_farine$s07bq03a, na.rm = TRUE), "\n")

menages_conso_farine <- conso_farine %>%
  distinct(hhid) %>%
  left_join(poids_menages, by = "hhid")

cat("Ménages consommateurs (farine/semoule) :", nrow(menages_conso_farine), "\n")

producteurs_mil <- s16d_raw %>%
  filter(s16dq01 == 1) %>%
  distinct(hhid)

menages_conso_farine %>%
  left_join(producteurs_mil %>% mutate(prod_mil = 1), by = "hhid") %>%
  summarise(
    nb_total = n(),
    nb_producteurs = sum(!is.na(prod_mil)),
    pct_producteurs = mean(!is.na(prod_mil)) * 100
  ) %>%
  print()

# --- Frais de mouture (s09b, code 217) ---
cat("\n--- 3.2 Frais de mouture ---\n")

mouture <- s09b_raw %>%
  filter(s09bq01 == 217, s09bq02 == 1)

cat("Nombre de ménages ayant déclaré des frais de mouture :", nrow(mouture), "\n")
cat("Montant dépensé (7 jours) : min =", min(mouture$s09bq03, na.rm = TRUE),
    " / médiane =", median(mouture$s09bq03, na.rm = TRUE),
    " / max =", max(mouture$s09bq03, na.rm = TRUE), "\n")

menages_mouture <- mouture %>% distinct(hhid)
cat("Nombre de ménages avec mouture :", nrow(menages_mouture), "\n")

menages_mouture %>%
  left_join(s07b_raw %>% filter(s07bq01 %in% c(7,14,15), s07bq02 == 1) %>% distinct(hhid) %>% mutate(conso_mil = 1), by = "hhid") %>%
  left_join(producteurs_mil %>% mutate(prod_mil = 1), by = "hhid") %>%
  summarise(
    nb_total = n(),
    nb_conso_mil = sum(!is.na(conso_mil)),
    nb_prod_mil = sum(!is.na(prod_mil)),
    nb_les_deux = sum(!is.na(conso_mil) & !is.na(prod_mil))
  ) %>%
  print()

# --- Entreprises non agricoles de transformation alimentaire ---
cat("\n--- 3.3 Entreprises non agricoles et transformation ---\n")

cat("Réponses positives par type d'entreprise (s10a) :\n")
s10a_raw %>%
  summarise(across(s10q02:s10q10, ~ sum(.x == 1, na.rm = TRUE))) %>%
  print()

cat("\nNombre total d'entreprises (lignes s10b) :", nrow(s10b_raw), "\n")

entreprises_transfo <- s10b_raw %>%
  filter(s10q17b %in% c(156, 157, 158) | s10q17a == 3)

cat("Entreprises de transformation alimentaire (codes 156, 157, 158 ou branche 3) :", nrow(entreprises_transfo), "\n")

cat("Distribution des sous-branches (s10q17b) :\n")
print(table(entreprises_transfo$s10q17b, useNA = "ifany"))

cat("Activités détaillées (s10q17c) :\n")
print(table(entreprises_transfo$s10q17c, useNA = "ifany"))

entreprises_transfo %>%
  left_join(producteurs_mil %>% mutate(prod_mil = 1), by = "hhid") %>%
  left_join(s07b_raw %>% filter(s07bq01 == 7, s07bq02 == 1) %>% distinct(hhid) %>% mutate(conso_mil = 1), by = "hhid") %>%
  summarise(
    nb_total = n(),
    nb_prod_mil = sum(!is.na(prod_mil)),
    nb_conso_mil = sum(!is.na(conso_mil))
  ) %>%
  print()

cat("\nExemples de biens/services produits par ces entreprises :\n")
entreprises_transfo %>%
  select(s10q16) %>%
  filter(!is.na(s10q16), s10q16 != "") %>%
  head(20) %>%
  print()

# --- Croisement global ---
cat("\n--- 3.4 Croisement producteurs/mouture/entreprise ---\n")

producteurs_mil %>%
  left_join(menages_mouture %>% mutate(mouture = 1), by = "hhid") %>%
  left_join(entreprises_transfo %>% distinct(hhid) %>% mutate(entreprise_transfo = 1), by = "hhid") %>%
  summarise(
    nb_producteurs = n(),
    nb_avec_mouture = sum(!is.na(mouture)),
    nb_avec_entreprise_transfo = sum(!is.na(entreprise_transfo)),
    nb_avec_les_deux = sum(!is.na(mouture) & !is.na(entreprise_transfo))
  ) %>%
  print()

cat("\nEDA terminée.\n")
cat("\nDécision : La transformation du mil est marginale (faible nombre de ménages consommateurs de farine/semoule, très peu d'entreprises dédiées).")
cat("\nLes indicateurs seront intégrés directement dans le profilage (Étape 5).\n")
sink()