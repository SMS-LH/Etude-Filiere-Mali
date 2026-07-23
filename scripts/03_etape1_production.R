# ==============================================================================
# 03_etape1_production.R
# Étape 1 – Production et rendements du mil
# Superficie proportionnelle, intrants, pratiques, capital humain,
# modèles OLS et effets fixes grappe.
# ==============================================================================

source(here::here("scripts", "00_setup.R"))

create_dir(here::here("outputs", "eda"))

# ------------------------------------------------------------------------------
# 1. Chargement des données
# ------------------------------------------------------------------------------

s00_raw  <- load_dta(paths$raw_menage, "s00_me_mli2021.dta")
s01_raw  <- load_dta(paths$raw_menage, "s01_me_mli2021.dta")
s02_raw  <- load_dta(paths$raw_menage, "s02_me_mli2021.dta")
s16a_raw <- load_dta(paths$raw_menage, "s16a_me_mli2021.dta")
s16b_raw <- load_dta(paths$raw_menage, "s16b_me_mli2021.dta")
s16c_raw <- load_dta(paths$raw_menage, "s16c_me_mli2021.dta")
s16d_raw <- load_dta(paths$raw_menage, "s16d_me_mli2021.dta")

s16c_conv <- load_rds(paths$processed, "s16c_converti.rds")
s16d_conv <- load_rds(paths$processed, "s16d_converti.rds")

pond <- load_dta(paths$raw_auxiliaires, "ehcvm_ponderations_mli2021.dta")

comm1 <- load_dta(paths$raw_communaute, "s01_co_mli2021.dta")
comm3 <- load_dta(paths$raw_communaute, "s03_co_mli2021.dta")

mali_regions <- st_read(here::here("data", "raw", "spatial", "gadm", "gadm41_MLI_1.shp"), quiet = TRUE)

# ------------------------------------------------------------------------------
# 2. Identifiants et poids
# ------------------------------------------------------------------------------

creer_hhid <- function(df) df %>% mutate(hhid = paste(grappe, menage, vague, sep = "_"))

s00_raw  <- creer_hhid(s00_raw)
s01_raw  <- creer_hhid(s01_raw)
s02_raw  <- creer_hhid(s02_raw)
s16a_raw <- creer_hhid(s16a_raw)
s16b_raw <- creer_hhid(s16b_raw)
s16c_raw <- creer_hhid(s16c_raw)
s16d_raw <- creer_hhid(s16d_raw)

poids_menages <- s00_raw %>%
  select(grappe, menage, vague, hhid) %>%
  left_join(pond, by = c("grappe", "menage")) %>%
  select(hhid, hhweight)

tous_menages <- poids_menages %>% distinct(hhid, hhweight)

# ==============================================================================
# EXPLORATION DES DONNÉES
# ==============================================================================

sink(here::here("outputs", "eda", "eda_etape1.txt"), split = TRUE)
cat("============================================================\n")
cat("EDA – Étape 1 : Production et rendement du mil\n")
cat("Date :", format(Sys.time()), "\n")
cat("============================================================\n\n")

cat("1. Superficies (méthode proportionnelle)\n")
cat("------------------------------------------\n")

s16c_ren <- s16c_raw %>%
  select(hhid, s16aq02 = s16cq02, s16aq03 = s16cq03, s16cq04, s16cq08)

parcelles_all <- s16c_ren %>%
  left_join(s16a_raw %>% select(hhid, s16aq02, s16aq03,
                                s16aq08,  # culture principale de la parcelle
                                sup_decl = s16aq09a, sup_decl_unite = s16aq09b,
                                sup_gps = s16aq47),
            by = c("hhid", "s16aq02", "s16aq03")) %>%
  mutate(
    sup_ha_parcelle = case_when(
      !is.na(sup_gps) & sup_gps > 0 ~ sup_gps,
      sup_decl_unite == 1 ~ sup_decl,
      sup_decl_unite == 2 ~ sup_decl / 10000,
      TRUE ~ NA_real_
    ),
    # Nouvelle règle : pourcentage manquant -> 100% uniquement si culture principale
    pct_culture = case_when(
      !is.na(s16cq08) & s16cq08 > 0 ~ s16cq08,
      s16cq04 == s16aq08 ~ 100,
      TRUE ~ NA_real_
    ),
    sup_culture_ha = sup_ha_parcelle * pct_culture / 100
  )

parcelles_mil <- parcelles_all %>% filter(s16cq04 == 1)

sup_menage_mil <- parcelles_mil %>%
  group_by(hhid) %>%
  summarise(superficie_mil = sum(sup_culture_ha, na.rm = TRUE), .groups = "drop")

cat(sprintf("Ménages avec superficie mil > 0 : %d\n", sum(sup_menage_mil$superficie_mil > 0)))
cat(sprintf("Superficie (ha) : min=%.4f  Q1=%.4f  médiane=%.4f  Q3=%.4f  max=%.4f\n",
            min(sup_menage_mil$superficie_mil, na.rm = TRUE),
            quantile(sup_menage_mil$superficie_mil, 0.25, na.rm = TRUE),
            median(sup_menage_mil$superficie_mil, na.rm = TRUE),
            quantile(sup_menage_mil$superficie_mil, 0.75, na.rm = TRUE),
            max(sup_menage_mil$superficie_mil, na.rm = TRUE)))
cat("Décision : superficie = GPS si disponible, sinon déclarée convertie en ha ; pourcentage manquant -> 100% uniquement si culture principale.\n\n")

cat("2. Production (s16d_conv)\n")
cat("-----------------------------\n")

prod_menage_mil <- s16d_conv %>%
  filter(s16dq01 == 1) %>%
  mutate(prod_totale_kg = coalesce(conso_kg,0) + coalesce(don_kg,0) +
           coalesce(vente_kg,0) + coalesce(stock_kg,0)) %>%
  group_by(hhid) %>%
  summarise(production_kg = sum(prod_totale_kg, na.rm = TRUE), .groups = "drop")

cat(sprintf("Ménages avec production > 0 : %d\n", sum(prod_menage_mil$production_kg > 0)))
cat(sprintf("Production (kg) : min=%.0f  médiane=%.0f  max=%.0f\n",
            min(prod_menage_mil$production_kg), median(prod_menage_mil$production_kg),
            max(prod_menage_mil$production_kg)))
cat("Décision : production = somme des usages convertis (table EAC).\n\n")

cat("3. Rendements\n")
cat("-------------\n")

rend_mil <- sup_menage_mil %>%
  inner_join(prod_menage_mil, by = "hhid") %>%
  filter(superficie_mil > 0, production_kg > 0) %>%
  mutate(rdt = production_kg / superficie_mil)

cat(sprintf("Avant winsorisation : %d ménages\n", nrow(rend_mil)))
cat(sprintf("  Min=%.1f  Q1=%.1f  Médiane=%.1f  Q3=%.1f  Max=%.1f\n",
            min(rend_mil$rdt), quantile(rend_mil$rdt, 0.25), median(rend_mil$rdt),
            quantile(rend_mil$rdt, 0.75), max(rend_mil$rdt)))
cat(sprintf("  Nb > 10 000 kg/ha : %d\n", sum(rend_mil$rdt > 10000)))
cat(sprintf("  Nb >  5 000 kg/ha : %d\n", sum(rend_mil$rdt > 5000)))

rend_mil <- rend_mil %>%
  mutate(rdt_w = winsorize(rdt, probs = c(0.01, 0.99)))

cat(sprintf("Après winsorisation (1%%-99%%) : Min=%.1f  Médiane=%.1f  Max=%.1f\n",
            min(rend_mil$rdt_w), median(rend_mil$rdt_w), max(rend_mil$rdt_w)))
cat("Décision : winsorisation simple, pas de plafond agronomique.\n\n")

cat("4. Intrants (s16b)\n")
cat("-------------------\n")

intrants_mil <- s16b_raw %>%
  filter(hhid %in% prod_menage_mil$hhid, s16bq02 == 1)

cat(sprintf("Nombre de lignes d'intrants : %d\n", nrow(intrants_mil)))
cat("Types d'intrants déclarés :\n")
print(table(intrants_mil$s16bq01))
cat("Décision : variables binaires (engrais inorganique, pesticides) et valeur achetée.\n\n")

cat("5. Autres variables\n")
cat("-------------------------------\n")

labour_fertilite_eda <- s16a_raw %>%
  filter(s16aq08 == 1) %>%
  group_by(hhid) %>%
  summarise(
    labour_manuel   = sum(s16aq44 == 2),
    labour_attele   = sum(s16aq44 == 3),
    labour_motorise = sum(s16aq44 == 4),
    fertilite_bonne  = sum(s16aq20 == 1),
    fertilite_moyenne = sum(s16aq20 == 2),
    fertilite_faible = sum(s16aq20 == 3),
    .groups = "drop"
  ) %>%
  mutate(
    labour_mode = case_when(
      labour_motorise >= labour_attele & labour_motorise >= labour_manuel ~ "motorise",
      labour_attele >= labour_manuel ~ "attele",
      TRUE ~ "manuel"
    ),
    fertilite = case_when(
      fertilite_bonne >= fertilite_moyenne & fertilite_bonne >= fertilite_faible ~ "bonne",
      fertilite_faible >= fertilite_moyenne ~ "faible",
      TRUE ~ "moyenne"
    )
  )

cat("Labour (majoritaire) :\n")
print(table(labour_fertilite_eda$labour_mode))
cat("Fertilité déclarée (majoritaire) :\n")
print(table(labour_fertilite_eda$fertilite))

chef_eda <- s01_raw %>%
  filter(s01q02 == 1, hhid %in% prod_menage_mil$hhid) %>%
  mutate(
    annee_naiss = as.numeric(s01q03c),
    annee_naiss = if_else(annee_naiss == 9999, NA_real_, annee_naiss),
    age_chef = 2021 - annee_naiss
  )
cat(sprintf("Âge chef (calculé) : min=%d  médiane=%d  max=%d  NA=%d\n",
            min(chef_eda$age_chef, na.rm=TRUE), median(chef_eda$age_chef, na.rm=TRUE),
            max(chef_eda$age_chef, na.rm=TRUE), sum(is.na(chef_eda$age_chef))))

educ_eda <- chef_eda %>%
  select(hhid, membres__id) %>%
  left_join(s02_raw %>% select(hhid, membres__id, s02q29), by = c("hhid", "membres__id")) %>%
  mutate(educ_formelle = as.integer(as.integer(s02q29) > 1 & !is.na(s02q29)))
cat(sprintf("Part éducation formelle (chef) : %.1f%%\n", mean(educ_eda$educ_formelle, na.rm=TRUE)*100))

cat("\nEDA terminée.\n")
sink()

# ==============================================================================
# PHASE 2 – BASE DE MODÉLISATION
# ==============================================================================

rend_mil <- rend_mil %>% left_join(poids_menages, by = "hhid")

intrants_menage <- s16b_raw %>%
  creer_hhid() %>%
  group_by(hhid) %>%
  summarise(
    valeur_intrants_fcfa = sum(as.numeric(s16bq09c), na.rm = TRUE),
    engrais_inorg = as.integer(any(as.integer(s16bq01) %in% 3:6 & as.integer(s16bq02) == 1, na.rm = TRUE)),
    pesticides    = as.integer(any(as.integer(s16bq01) %in% 7:10 & as.integer(s16bq02) == 1, na.rm = TRUE)),
    engrais_org   = as.integer(any(as.integer(s16bq01) %in% 1:2 & as.integer(s16bq02) == 1, na.rm = TRUE)),
    .groups = "drop"
  )

semences_mil <- s16c_raw %>%
  filter(s16cq04 == 1) %>%
  group_by(hhid) %>%
  summarise(semence_amelioree = as.integer(any(as.integer(s16cq09) %in% c(2,3), na.rm = TRUE)),
            .groups = "drop")

irrigation_mil <- s16a_raw %>%
  filter(s16aq08 == 1) %>%
  group_by(hhid) %>%
  summarise(irrigation = as.integer(any(as.integer(s16aq17) %in% c(1,2,3), na.rm = TRUE)),
            .groups = "drop")

cols_travail <- c(paste0("s16aq33b_", 1:31), paste0("s16aq35b_", 1:31),
                  paste0("s16aq39b_", 1:4), paste0("s16aq41b_", 1:4))
cols_travail <- intersect(cols_travail, names(s16a_raw))

travail_mil <- s16a_raw %>%
  filter(s16aq08 == 1) %>%
  mutate(across(all_of(cols_travail), as.numeric)) %>%
  rowwise() %>%
  mutate(jours_parcelle = sum(c_across(all_of(cols_travail)), na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(hhid) %>%
  summarise(jours_travail = sum(jours_parcelle, na.rm = TRUE), .groups = "drop")

chef <- s01_raw %>%
  filter(s01q02 == 1) %>%
  mutate(
    annee_naiss = as.numeric(s01q03c),
    annee_naiss = if_else(annee_naiss == 9999, NA_real_, annee_naiss),
    age_chef = 2021 - annee_naiss,
    femme_chef = as.integer(as.integer(s01q01) == 2)
  )

educ_chef <- chef %>%
  select(hhid, membres__id) %>%
  left_join(s02_raw %>% select(hhid, membres__id, s02q29, s02q01__1, s02q01__2, s02q01__3),
            by = c("hhid", "membres__id")) %>%
  mutate(
    educ_formelle = as.integer(as.integer(s02q29) > 1 & !is.na(s02q29)),
    sait_lire = as.integer(
      as.integer(s02q01__1) == 1 | as.integer(s02q01__2) == 1 | as.integer(s02q01__3) == 1
    ),
    sait_lire = if_else(is.na(sait_lire), 0L, sait_lire)
  ) %>%
  select(hhid, educ_formelle, sait_lire)

chef <- chef %>% left_join(educ_chef, by = "hhid")

taille_menage <- s01_raw %>%
  group_by(hhid) %>%
  summarise(taille_menage = n(), .groups = "drop")

labour_fertilite <- s16a_raw %>%
  filter(s16aq08 == 1) %>%
  group_by(hhid) %>%
  summarise(
    labour_manuel   = sum(s16aq44 == 2),
    labour_attele   = sum(s16aq44 == 3),
    labour_motorise = sum(s16aq44 == 4),
    fertilite_bonne  = sum(s16aq20 == 1),
    fertilite_moyenne = sum(s16aq20 == 2),
    fertilite_faible = sum(s16aq20 == 3),
    .groups = "drop"
  ) %>%
  mutate(
    labour_mode = case_when(
      labour_motorise >= labour_attele & labour_motorise >= labour_manuel ~ "motorise",
      labour_attele >= labour_manuel ~ "attele",
      TRUE ~ "manuel"
    ),
    fertilite = case_when(
      fertilite_bonne >= fertilite_moyenne & fertilite_bonne >= fertilite_faible ~ "bonne",
      fertilite_faible >= fertilite_moyenne ~ "faible",
      TRUE ~ "moyenne"
    )
  )

comm_vars <- comm1 %>%
  select(grappe, vague, dist_ville = s01q05, electricite = s01q11) %>%
  left_join(
    comm3 %>% select(grappe, vague, irrigation_comm = s03q17,
                     vulgarisation = s03q16, engrais_comm = s03q12),
    by = c("grappe", "vague")
  ) %>%
  mutate(
    electricite     = if_else(electricite == 1, 1L, 0L),
    irrigation_comm = if_else(irrigation_comm == 1, 1L, 0L),
    vulgarisation   = if_else(vulgarisation == 1, 1L, 0L),
    engrais_comm    = if_else(engrais_comm == 1, 1L, 0L)
  )

base_mod <- rend_mil %>%
  left_join(intrants_menage, by = "hhid") %>%
  left_join(semences_mil, by = "hhid") %>%
  left_join(irrigation_mil, by = "hhid") %>%
  left_join(travail_mil, by = "hhid") %>%
  left_join(chef %>% select(hhid, age_chef, femme_chef, educ_formelle, sait_lire), by = "hhid") %>%
  left_join(taille_menage, by = "hhid") %>%
  left_join(labour_fertilite, by = "hhid") %>%
  left_join(s00_raw %>% select(hhid, region = s00q01, milieu = s00q04), by = "hhid") %>%
  left_join(s00_raw %>% select(hhid, grappe, vague), by = "hhid") %>%
  left_join(comm_vars, by = c("grappe", "vague")) %>%
  mutate(
    across(c(valeur_intrants_fcfa, engrais_inorg, pesticides, engrais_org,
             semence_amelioree, irrigation, jours_travail,
             educ_formelle, sait_lire, age_chef, femme_chef, taille_menage),
           ~ if_else(is.na(.x), 0, as.numeric(.x))),
    ln_intrants = log(valeur_intrants_fcfa + 1),
    ln_travail  = log(jours_travail + 1),
    labour_mode = factor(labour_mode, levels = c("manuel", "attele", "motorise")),
    fertilite   = factor(fertilite, levels = c("moyenne", "bonne", "faible")),
    region      = factor(region,
                         levels = 1:9,
                         labels = c("Kayes","Koulikoro","Sikasso","Ségou",
                                    "Mopti","Timbuktu","Gao","Kidal","Bamako")),
    milieu      = factor(milieu, levels = c(1,2), labels = c("Urbain", "Rural")),
    log_rdt     = log(rdt_w)
  )

# ==============================================================================
# PHASE 3 – MODÉLISATION
# ==============================================================================

cat("\nEstimation des modèles de rendement...\n")

m1 <- feols(
  log_rdt ~ ln_intrants + engrais_inorg + pesticides +
    semence_amelioree + irrigation +
    labour_mode + fertilite +
    educ_formelle + sait_lire + age_chef + femme_chef + taille_menage +
    ln_travail +
    region + milieu +
    dist_ville + electricite + irrigation_comm + vulgarisation + engrais_comm,
  data = base_mod,
  weights = ~ hhweight,
  cluster = ~ grappe
)

m2 <- feols(
  log_rdt ~ ln_intrants + engrais_inorg + pesticides +
    semence_amelioree + irrigation +
    labour_mode + fertilite +
    educ_formelle + sait_lire + age_chef + femme_chef + taille_menage +
    ln_travail |
    grappe,
  data = base_mod,
  weights = ~ hhweight,
  cluster = ~ grappe
)

noms_renommes <- c(
  "ln_intrants"       = "Log(intrants achetés + 1)",
  "engrais_inorg"     = "Utilise engrais inorganique",
  "pesticides"        = "Utilise pesticides",
  "semence_amelioree" = "Utilise semences améliorées",
  "irrigation"        = "Irrigation (parcelle)",
  "labour_modeattele"   = "Labour attelé",
  "labour_modemotorise" = "Labour motorisé",
  "fertilitebonne"      = "Fertilité bonne",
  "fertilitefaible"     = "Fertilité faible",
  "educ_formelle"     = "Chef éduqué (formel)",
  "sait_lire"         = "Chef sait lire",
  "age_chef"          = "Âge chef (années)",
  "femme_chef"        = "Chef féminin",
  "taille_menage"     = "Taille du ménage",
  "ln_travail"        = "Log(jours travail + 1)",
  "regionKoulikoro"   = "Région Koulikoro",
  "regionSikasso"     = "Région Sikasso",
  "regionSégou"       = "Région Ségou",
  "regionMopti"       = "Région Mopti",
  "regionTimbuktu"    = "Région Tombouctou",
  "regionGao"         = "Région Gao",
  "regionKidal"       = "Région Kidal",
  "regionBamako"      = "Région Bamako",
  "milieuRural"       = "Milieu rural",
  "dist_ville"        = "Distance ville (km)",
  "electricite"       = "Accès électricité",
  "irrigation_comm"   = "Irrigation (communauté)",
  "vulgarisation"     = "Vulgarisation agricole",
  "engrais_comm"      = "Engrais chimique (communauté)"
)

tab_mod_complet <- modelsummary(
  list("OLS complet" = m1, "Effets fixes grappe" = m2),
  output = "data.frame",
  coef_rename = noms_renommes,
  stars = TRUE,
  gof_map = c("nobs", "r.squared", "rmse")
)

saveRDS(tab_mod_complet, here::here("data", "processed", "modeles_rendement_mil.rds"))
cat("Tableau complet des modèles sauvegardé dans data/processed/modeles_rendement_mil.rds\n")

# --- Tableau simplifié pour Excel ---
tab_simple <- tab_mod_complet %>%
  filter(statistic == "estimate") %>%
  select(term, `OLS complet`, `Effets fixes grappe`) %>%
  mutate(term = dplyr::recode(term, !!!noms_renommes)) %>%
  rename(Variable = term, OLS = `OLS complet`, `Effets fixes` = `Effets fixes grappe`) %>%
  filter(Variable != "(Intercept)")

print(tab_simple)

# ==============================================================================
# EXPORT EXCEL UNIQUE
# ==============================================================================

wb <- createWorkbook()

titre_style  <- createStyle(fontSize = 14, fontColour = "#2E4053", textDecoration = "bold")
header_style <- createStyle(fontColour = "#ffffff", fgFill = "#4B5320",
                            halign = "center", textDecoration = "bold",
                            border = "TopBottomLeftRight")
body_style   <- createStyle(halign = "left", border = "TopBottomLeftRight")

# --- Feuille 1 : Rendement par région ---
rdt_region <- base_mod %>%
  group_by(region) %>%
  summarise(Rendement_moyen = round(weighted.mean(rdt_w, hhweight, na.rm = TRUE), 0),
            Nombre_menages = n(),
            .groups = "drop") %>%
  rename(Région = region)

addWorksheet(wb, "Rendement par région")
writeData(wb, "Rendement par région", "Rendement moyen du mil par région", startCol = 1, startRow = 1)
mergeCells(wb, "Rendement par région", cols = 1:3, rows = 1)
addStyle(wb, "Rendement par région", titre_style, rows = 1, cols = 1:3, gridExpand = TRUE)
writeData(wb, "Rendement par région", rdt_region, startRow = 3)
addStyle(wb, "Rendement par région", header_style, rows = 3, cols = 1:3, gridExpand = TRUE)
addStyle(wb, "Rendement par région", body_style, rows = 4:(4+nrow(rdt_region)-1), cols = 1:3, gridExpand = TRUE)
setColWidths(wb, "Rendement par région", cols = 1:3, widths = c(20, 20, 15))

# --- Feuille 2 : Rendement par mode de labour ---
rdt_labour <- base_mod %>%
  group_by(labour_mode) %>%
  summarise(Rendement_moyen = round(weighted.mean(rdt_w, hhweight, na.rm = TRUE), 0),
            Nombre_menages = n(),
            .groups = "drop") %>%
  rename(Mode_labour = labour_mode)

addWorksheet(wb, "Rendement par labour")
writeData(wb, "Rendement par labour", "Rendement moyen du mil par mode de labour", startCol = 1, startRow = 1)
mergeCells(wb, "Rendement par labour", cols = 1:3, rows = 1)
addStyle(wb, "Rendement par labour", titre_style, rows = 1, cols = 1:3, gridExpand = TRUE)
writeData(wb, "Rendement par labour", rdt_labour, startRow = 3)
addStyle(wb, "Rendement par labour", header_style, rows = 3, cols = 1:3, gridExpand = TRUE)
addStyle(wb, "Rendement par labour", body_style, rows = 4:(4+nrow(rdt_labour)-1), cols = 1:3, gridExpand = TRUE)
setColWidths(wb, "Rendement par labour", cols = 1:3, widths = c(20, 20, 15))

# --- Feuille 3 : Rendement par fertilité déclarée ---
rdt_fert <- base_mod %>%
  group_by(fertilite) %>%
  summarise(Rendement_moyen = round(weighted.mean(rdt_w, hhweight, na.rm = TRUE), 0),
            Nombre_menages = n(),
            .groups = "drop") %>%
  rename(Fertilité = fertilite)

addWorksheet(wb, "Rendement par fertilité")
writeData(wb, "Rendement par fertilité", "Rendement moyen du mil par fertilité déclarée", startCol = 1, startRow = 1)
mergeCells(wb, "Rendement par fertilité", cols = 1:3, rows = 1)
addStyle(wb, "Rendement par fertilité", titre_style, rows = 1, cols = 1:3, gridExpand = TRUE)
writeData(wb, "Rendement par fertilité", rdt_fert, startRow = 3)
addStyle(wb, "Rendement par fertilité", header_style, rows = 3, cols = 1:3, gridExpand = TRUE)
addStyle(wb, "Rendement par fertilité", body_style, rows = 4:(4+nrow(rdt_fert)-1), cols = 1:3, gridExpand = TRUE)
setColWidths(wb, "Rendement par fertilité", cols = 1:3, widths = c(20, 20, 15))

# --- Feuille 4 : Modèles (tableau simplifié) ---
addWorksheet(wb, "Modèles")
writeData(wb, "Modèles", "Déterminants du rendement du mil (coefficients simplifiés)", startCol = 1, startRow = 1)
mergeCells(wb, "Modèles", cols = 1:3, rows = 1)
addStyle(wb, "Modèles", titre_style, rows = 1, cols = 1:3, gridExpand = TRUE)

writeData(wb, "Modèles", tab_simple, startRow = 3)
addStyle(wb, "Modèles", header_style, rows = 3, cols = 1:3, gridExpand = TRUE)
addStyle(wb, "Modèles", body_style, rows = 4:(4+nrow(tab_simple)-1), cols = 1:3, gridExpand = TRUE)
setColWidths(wb, "Modèles", cols = 1:3, widths = c(30, 20, 20))

# --- Feuille 5 : Dictionnaire ---
addWorksheet(wb, "Dictionnaire")
writeData(wb, "Dictionnaire", "Dictionnaire des variables", startCol = 1, startRow = 1)
mergeCells(wb, "Dictionnaire", cols = 1:2, rows = 1)
addStyle(wb, "Dictionnaire", titre_style, rows = 1, cols = 1:2, gridExpand = TRUE)

dico <- data.frame(
  Variable = names(noms_renommes),
  Description = noms_renommes
)
writeData(wb, "Dictionnaire", dico, startRow = 3)
addStyle(wb, "Dictionnaire", header_style, rows = 3, cols = 1:2, gridExpand = TRUE)
addStyle(wb, "Dictionnaire", body_style, rows = 4:(4+nrow(dico)-1), cols = 1:2, gridExpand = TRUE)
setColWidths(wb, "Dictionnaire", cols = 1:2, widths = c(30, 60))

saveWorkbook(wb, here::here("outputs", "tables", "resultats_etape1.xlsx"), overwrite = TRUE)

# ==============================================================================
# CARTOGRAPHIE
# ==============================================================================

cat("\n--- Cartographie ---\n")

rdt_region_carte <- base_mod %>%
  group_by(region) %>%
  summarise(rdt_moyen = weighted.mean(rdt_w, hhweight, na.rm = TRUE), .groups = "drop")

carte_data <- mali_regions %>%
  select(NAME_1, geometry) %>%
  left_join(rdt_region_carte, by = c("NAME_1" = "region"))

centroides <- carte_data %>%
  st_centroid() %>%
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2],
         etiquette = ifelse(is.na(rdt_moyen), NAME_1,
                            paste0(NAME_1, "\n", round(rdt_moyen), " kg/ha")))

carte <- ggplot(carte_data) +
  geom_sf(aes(fill = rdt_moyen), color = "white", linewidth = 0.3) +
  scale_fill_gradient(low = "#fee6ce", high = "#a63603",
                      na.value = "grey90", name = "Rendement\n(kg/ha)") +
  geom_text(data = centroides, aes(x = lon, y = lat, label = etiquette),
            size = 3, fontface = "bold", lineheight = 0.9) +
  labs(title = "Rendement moyen du mil par région",
       subtitle = "Campagne 2021/22 – EHCVM-2 Mali",
       caption = "Source : EHCVM-2. Rendement winsorisé (1%-99%).") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank(),
        panel.grid = element_blank())

ggsave(file.path(paths$maps, "carte_rendement_mil.png"),
       carte, width = 8, height = 7, dpi = 150, bg = "white")

cat("Carte sauvegardée dans outputs/maps/carte_rendement_mil.png\n")
cat("\nÉtape 1 terminée.\n")

saveRDS(m1, here::here("data", "processed", "modele_rendement_ols.rds"))