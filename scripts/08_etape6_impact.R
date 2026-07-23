# ==============================================================================
# 08_etape6_impact.R
# Étape 6 – Impact de la filière mil sur la sécurité alimentaire
# ==============================================================================

source(here::here("scripts", "00_setup.R"))

create_dir(here::here("outputs", "eda"))

# ------------------------------------------------------------------------------
# 1. Chargement des données
# ------------------------------------------------------------------------------

base <- load_rds(paths$processed, "typologie_mil.rds")

# Ajout de l'âge du chef (calculé depuis l'année de naissance pour éviter les NA)
s01_raw <- load_dta(paths$raw_menage, "s01_me_mli2021.dta")
s01_raw <- s01_raw %>%
  mutate(hhid = paste(grappe, menage, vague, sep = "_"))

chef_age <- s01_raw %>%
  filter(s01q02 == 1) %>%
  mutate(
    annee_naiss = as.numeric(s01q03c),
    annee_naiss = if_else(annee_naiss == 9999, NA_real_, annee_naiss),
    age_chef_calc = 2021 - annee_naiss
  ) %>%
  select(hhid, age_chef_calc)

base <- base %>%
  left_join(chef_age, by = "hhid") %>%
  mutate(age_chef = if_else(!is.na(age_chef_calc), age_chef_calc, age_chef))

# Ajout du revenu agricole total (pour les modèles incluant la commercialisation)
s16d_raw <- load_dta(paths$raw_menage, "s16d_me_mli2021.dta")
s16d_raw <- s16d_raw %>%
  mutate(hhid = paste(grappe, menage, vague, sep = "_"))

revenu_total <- s16d_raw %>%
  filter(s16dq04 == 1 | s16dq06 > 0) %>%
  group_by(hhid) %>%
  summarise(revenu_agri_total = sum(s16dq06, na.rm = TRUE), .groups = "drop")

base <- base %>%
  left_join(revenu_total, by = "hhid") %>%
  mutate(revenu_agri_total = if_else(is.na(revenu_agri_total), 0, revenu_agri_total))

# ------------------------------------------------------------------------------
# 2. Préparation des variables
# ------------------------------------------------------------------------------

base <- base %>%
  mutate(
    # Variables d'intérêt : déjà producteur_mil et consommateur_mil
    log_revenu_agri = log(revenu_agri_total + 1),
    educ_chef_bin   = if_else(educ_chef > 1 & educ_chef != 9999, 1L, 0L),
    ln_pcexp        = log(pcexp + 1),
    milieu          = factor(milieu, levels = c(1, 2), labels = c("Urbain", "Rural")),
    region          = factor(region)
  )

# ==============================================================================
# EXPLORATION DES DONNÉES
# ==============================================================================

sink(here::here("outputs", "eda", "eda_etape6.txt"), split = TRUE)
cat("============================================================\n")
cat("EDA – Étape 6 : Impact de la filière mil sur la sécurité alimentaire\n")
cat("Date :", format(Sys.time()), "\n")
cat("============================================================\n\n")

cat("1. Sécurité alimentaire (FIES et HDDS)\n")
cat("----------------------------------------\n")
cat(sprintf("Score FIES (0-8) : min=%d  Q1=%d  médiane=%d  Q3=%d  max=%d  moyenne=%.2f  NA=%d\n",
            min(base$fies_score, na.rm = TRUE),
            quantile(base$fies_score, 0.25, na.rm = TRUE),
            median(base$fies_score, na.rm = TRUE),
            quantile(base$fies_score, 0.75, na.rm = TRUE),
            max(base$fies_score, na.rm = TRUE),
            mean(base$fies_score, na.rm = TRUE),
            sum(is.na(base$fies_score))))
cat(sprintf("Score HDDS (0-12) : min=%d  Q1=%d  médiane=%d  Q3=%d  max=%d  moyenne=%.2f  NA=%d\n",
            min(base$hdds_score, na.rm = TRUE),
            quantile(base$hdds_score, 0.25, na.rm = TRUE),
            median(base$hdds_score, na.rm = TRUE),
            quantile(base$hdds_score, 0.75, na.rm = TRUE),
            max(base$hdds_score, na.rm = TRUE),
            mean(base$hdds_score, na.rm = TRUE),
            sum(is.na(base$hdds_score))))

cat("\n2. Participation à la filière mil\n")
cat("----------------------------------\n")
cat(sprintf("Producteurs de mil : %d (%.1f%%)\n", sum(base$producteur_mil == 1),
            100 * mean(base$producteur_mil == 1)))
cat(sprintf("Consommateurs de mil : %d (%.1f%%)\n", sum(base$consommateur_mil == 1),
            100 * mean(base$consommateur_mil == 1)))

# Croisement FIES/HDDS selon le statut
cat("\nScore FIES moyen par statut :\n")
base %>%
  group_by(groupe) %>%
  summarise(FIES_moyen = mean(fies_score, na.rm = TRUE),
            HDDS_moyen = mean(hdds_score, na.rm = TRUE),
            n = n(), .groups = "drop") %>%
  print()

cat("\n3. Variables de contrôle (aperçu)\n")
cat("-----------------------------------\n")
cat(sprintf("Âge chef (calculé) : min=%d  médiane=%d  max=%d  NA=%d\n",
            min(base$age_chef, na.rm = TRUE), median(base$age_chef, na.rm = TRUE),
            max(base$age_chef, na.rm = TRUE), sum(is.na(base$age_chef))))
cat(sprintf("Éducation chef (binaire) : %.1f%% avec éducation formelle\n",
            100 * mean(base$educ_chef_bin, na.rm = TRUE)))
cat(sprintf("Taille du ménage : min=%d  médiane=%d  max=%d\n",
            min(base$hhsize, na.rm = TRUE), median(base$hhsize, na.rm = TRUE),
            max(base$hhsize, na.rm = TRUE)))
cat(sprintf("Log dépense/tête : moyenne=%.2f  NA=%d\n",
            mean(base$ln_pcexp, na.rm = TRUE), sum(is.na(base$ln_pcexp))))

cat("\nEDA terminée.\n")
sink()

# ==============================================================================
# MODÉLISATION ÉCONOMÉTRIQUE
# ==============================================================================

cat("\nEstimation des modèles d'impact...\n")

# Modèle 1 : Impact sur le FIES (avec effets fixes région)
m1 <- feols(
  fies_score ~ producteur_mil + consommateur_mil +
    age_chef + educ_chef_bin + hhsize + ln_pcexp + milieu +
    superficie_totale + nb_bovins + nb_ovins_caprins + nb_equipements +
    acces_credit + acces_cooperative + nb_chocs |
    region,
  data = base,
  weights = ~ hhweight,
  cluster = ~ grappe
)

# Modèle 2 : Impact sur le HDDS (avec effets fixes région)
m2 <- feols(
  hdds_score ~ producteur_mil + consommateur_mil +
    age_chef + educ_chef_bin + hhsize + ln_pcexp + milieu +
    superficie_totale + nb_bovins + nb_ovins_caprins + nb_equipements +
    acces_credit + acces_cooperative + nb_chocs |
    region,
  data = base,
  weights = ~ hhweight,
  cluster = ~ grappe
)

# --- Tableau simplifié des résultats ---

noms_renommes <- c(
  "producteur_mil"      = "Producteur de mil",
  "consommateur_mil"    = "Consommateur de mil",
  "age_chef"            = "Âge du chef (années)",
  "educ_chef_bin"       = "Chef éduqué (formel)",
  "hhsize"              = "Taille du ménage",
  "ln_pcexp"            = "Log(dépense/tête + 1)",
  "milieuRural"         = "Milieu rural",
  "superficie_totale"   = "Superficie agricole (ha)",
  "nb_bovins"           = "Nombre de bovins",
  "nb_ovins_caprins"    = "Ovins/caprins",
  "nb_equipements"      = "Équipements agricoles",
  "acces_credit"        = "Accès au crédit",
  "acces_cooperative"   = "Coopérative dans la communauté",
  "nb_chocs"            = "Nombre de chocs subis"
)

# Tableau complet (sauvegardé en RDS)
tab_complet <- modelsummary(
  list("FIES" = m1, "HDDS" = m2),
  output = "data.frame",
  coef_rename = noms_renommes,
  stars = TRUE,
  gof_map = c("nobs", "r.squared", "rmse")
)
saveRDS(tab_complet, here::here("data", "processed", "modeles_impact_mil.rds"))

# Tableau simplifié pour Excel (coefficients uniquement)
tab_simple <- tab_complet %>%
  filter(statistic == "estimate") %>%
  select(term, FIES, HDDS) %>%
  mutate(term = dplyr::recode(term, !!!noms_renommes)) %>%
  rename(Variable = term) %>%
  filter(!is.na(FIES) | !is.na(HDDS),
         Variable != "(Intercept)")

print(tab_simple)

# ==============================================================================
# EXPORT EXCEL
# ==============================================================================

wb <- createWorkbook()

titre_style  <- createStyle(fontSize = 14, fontColour = "#2E4053", textDecoration = "bold")
header_style <- createStyle(fontColour = "#ffffff", fgFill = "#4B5320",
                            halign = "center", textDecoration = "bold",
                            border = "TopBottomLeftRight")
body_style   <- createStyle(halign = "left", border = "TopBottomLeftRight")

# --- Feuille 1 : Indicateurs descriptifs ---
desc <- base %>%
  group_by(groupe) %>%
  summarise(
    Effectif = n(),
    FIES_moyen = round(mean(fies_score, na.rm = TRUE), 2),
    HDDS_moyen = round(mean(hdds_score, na.rm = TRUE), 2),
    Part_producteurs = round(mean(producteur_mil) * 100, 1),
    Part_consommateurs = round(mean(consommateur_mil) * 100, 1),
    .groups = "drop"
  )

addWorksheet(wb, "Statistiques descriptives")
writeData(wb, "Statistiques descriptives", "Sécurité alimentaire selon le statut vis-à-vis du mil", startCol = 1, startRow = 1)
mergeCells(wb, "Statistiques descriptives", cols = 1:ncol(desc), rows = 1)
addStyle(wb, "Statistiques descriptives", titre_style, rows = 1, cols = 1:ncol(desc), gridExpand = TRUE)
writeData(wb, "Statistiques descriptives", desc, startRow = 3)
addStyle(wb, "Statistiques descriptives", header_style, rows = 3, cols = 1:ncol(desc), gridExpand = TRUE)
addStyle(wb, "Statistiques descriptives", body_style, rows = 4:(4+nrow(desc)-1), cols = 1:ncol(desc), gridExpand = TRUE)
setColWidths(wb, "Statistiques descriptives", cols = 1:ncol(desc), widths = "auto")

# --- Feuille 2 : Modèles ---
addWorksheet(wb, "Modèles")
writeData(wb, "Modèles", "Impact de la filière mil sur la sécurité alimentaire", startCol = 1, startRow = 1)
mergeCells(wb, "Modèles", cols = 1:3, rows = 1)
addStyle(wb, "Modèles", titre_style, rows = 1, cols = 1:3, gridExpand = TRUE)

writeData(wb, "Modèles", tab_simple, startRow = 3)
addStyle(wb, "Modèles", header_style, rows = 3, cols = 1:3, gridExpand = TRUE)
addStyle(wb, "Modèles", body_style, rows = 4:(4+nrow(tab_simple)-1), cols = 1:3, gridExpand = TRUE)
setColWidths(wb, "Modèles", cols = 1:3, widths = c(30, 20, 20))

# --- Feuille 3 : Dictionnaire ---
dico <- data.frame(
  Variable = c("FIES_moyen","HDDS_moyen","producteur_mil","consommateur_mil",
               "age_chef","educ_chef_bin","hhsize","ln_pcexp","milieu",
               "superficie_totale","nb_bovins","nb_ovins_caprins","nb_equipements",
               "acces_credit","acces_cooperative","nb_chocs"),
  Description = c(
    "Score d'insécurité alimentaire (FIES, 0-8)",
    "Score de diversité alimentaire (HDDS, 0-12)",
    "Ménage producteur de mil (1=oui)",
    "Ménage consommateur de mil (1=oui)",
    "Âge du chef de ménage (années)",
    "Chef avec au moins un niveau primaire (1=oui)",
    "Taille du ménage",
    "Logarithme des dépenses par tête + 1",
    "Milieu de résidence (Urbain/Rural)",
    "Superficie agricole totale (ha)",
    "Nombre de bovins possédés",
    "Nombre d'ovins et caprins",
    "Nombre d'équipements agricoles",
    "Accès au crédit (1=oui)",
    "Présence d'une coopérative dans la communauté (1=oui)",
    "Nombre de chocs subis par le ménage"
  )
)

addWorksheet(wb, "Dictionnaire")
writeData(wb, "Dictionnaire", "Dictionnaire des variables", startCol = 1, startRow = 1)
mergeCells(wb, "Dictionnaire", cols = 1:2, rows = 1)
addStyle(wb, "Dictionnaire", titre_style, rows = 1, cols = 1:2, gridExpand = TRUE)
writeData(wb, "Dictionnaire", dico, startRow = 3)
addStyle(wb, "Dictionnaire", header_style, rows = 3, cols = 1:2, gridExpand = TRUE)
addStyle(wb, "Dictionnaire", body_style, rows = 4:(4+nrow(dico)-1), cols = 1:2, gridExpand = TRUE)
setColWidths(wb, "Dictionnaire", cols = 1:2, widths = "auto")

saveWorkbook(wb, here::here("outputs", "tables", "resultats_etape6.xlsx"), overwrite = TRUE)

cat("\nÉtape 6 terminée.\n")