# ==============================================================================
# 02_etape0_choix_produit.R
# Étape 0 – Comparaison de quatre céréales (mil, sorgho, riz, maïs)
# ==============================================================================

source(here::here("scripts", "00_setup.R"))
create_dir(here::here("outputs", "eda"))
create_dir(here::here("outputs", "figures"))

# ------------------------------------------------------------------------------
# 1. Chargement des données
# ------------------------------------------------------------------------------

s00_raw  <- load_dta(paths$raw_menage, "s00_me_mli2021.dta")
s07b_raw <- load_dta(paths$raw_menage, "s07b_me_mli2021.dta")
s16a_raw <- load_dta(paths$raw_menage, "s16a_me_mli2021.dta")

s16c_conv <- load_rds(paths$processed, "s16c_converti.rds")
s16d_conv <- load_rds(paths$processed, "s16d_converti.rds")

pond      <- load_dta(paths$raw_auxiliaires, "ehcvm_ponderations_mli2021.dta")
conso_agr <- load_dta(paths$raw_auxiliaires, "ehcvm_conso_mli2021.dta")
nsu_raw   <- load_dta(paths$raw_auxiliaires, "ehcvm_nsu_mli2021.dta")

faostat_prod  <- readxl::read_excel(here::here("data", "raw", "faostat", "FAOSTAT_mali_production.xls"))
faostat_trade <- readxl::read_excel(here::here("data", "raw", "faostat", "FAOSTAT_mali_trade.xls"))

# ------------------------------------------------------------------------------
# 2. Identifiants, poids, correspondances
# ------------------------------------------------------------------------------

creer_hhid <- function(df) df %>% mutate(hhid = paste(grappe, menage, vague, sep = "_"))

s00_raw  <- creer_hhid(s00_raw)
s07b_raw <- creer_hhid(s07b_raw)
s16a_raw <- creer_hhid(s16a_raw)

poids_menages <- s00_raw %>%
  select(grappe, menage, vague, hhid) %>%
  left_join(pond, by = c("grappe", "menage")) %>%
  select(hhid, hhweight)

tous_menages <- poids_menages %>% distinct(hhid, hhweight)

groupes <- list(
  "Mil"    = list(conso = c(7, 14, 15),      prod = 1),
  "Sorgho" = list(conso = c(8),              prod = 2),
  "Riz"    = list(conso = c(1, 2, 3, 4, 19), prod = 3),
  "Maïs"   = list(conso = c(5, 6, 12, 13),   prod = 4)
)

# ==============================================================================
# EXPLORATION DES DONNÉES (EDA)
# ==============================================================================

sink(here::here("outputs", "eda", "eda_etape0.txt"), split = TRUE)
cat("============================================================\n")
cat("EDA – Étape 0 : Qualité des données pour mil, sorgho, riz, maïs\n")
cat("Date :", format(Sys.time()), "\n")
cat("============================================================\n\n")

# --- 1. Poids de sondage ---
cat("1. Poids de sondage (hhweight)\n")
cat("---------------------------------\n")
cat(sprintf("  Min    : %.2f\n", min(tous_menages$hhweight)))
cat(sprintf("  Max    : %.2f\n", max(tous_menages$hhweight)))
cat(sprintf("  Moyenne: %.1f\n", mean(tous_menages$hhweight)))
cat(sprintf("  NAs    : %d\n\n", sum(is.na(tous_menages$hhweight))))

# --- 2. Superficies brutes (s16a) ---
cat("2. Superficies des parcelles (s16a)\n")
cat("-------------------------------------\n")
parcelles_cand <- s16a_raw %>%
  filter(s16aq08 %in% c(1,2,3,4)) %>%
  mutate(culture = case_when(
    s16aq08 == 1 ~ "Mil", s16aq08 == 2 ~ "Sorgho", s16aq08 == 3 ~ "Riz", s16aq08 == 4 ~ "Maïs"
  ))

cat(sprintf("Nombre total de parcelles candidates : %d\n", nrow(parcelles_cand)))
cat("Répartition des unités de superficie (s16aq09b) :\n")
print(table(parcelles_cand$s16aq09b, useNA = "ifany"))
cat("\nSuperficie déclarée (s16aq09a) par culture :\n")
for (cult in c("Mil","Sorgho","Riz","Maïs")) {
  sub <- parcelles_cand %>% filter(culture == cult)
  cat(sprintf("  %-8s : %4d parcelles\n", cult, nrow(sub)))
  cat(sprintf("         Min=%7.2f  Médiane=%7.2f  Max=%7.2f  NA=%d\n",
              min(sub$s16aq09a, na.rm=TRUE),
              median(sub$s16aq09a, na.rm=TRUE),
              max(sub$s16aq09a, na.rm=TRUE),
              sum(is.na(sub$s16aq09a))))
  cat(sprintf("         Parcelles avec GPS disponible : %d\n\n",
              sum(!is.na(sub$s16aq47) & sub$s16aq47 > 0)))
}
cat("Décision : superficie retenue = GPS si disponible, sinon déclarée convertie en ha.\n")
cat("          La méthode 'culture principale' est utilisée pour le classement des cultures.\n\n")

# --- 3. Quantités récoltées (s16c converti) ---
cat("3. Quantités récoltées converties en kg (s16c_conv)\n")
cat("-----------------------------------------------------\n")
cat("(Conversion réalisée avec la table externe EAC)\n")
for (cult in c("Mil","Sorgho","Riz","Maïs")) {
  code <- groupes[[cult]]$prod
  sub <- s16c_conv %>% filter(s16cq04 == code)
  cat(sprintf("  %-8s : %5d lignes\n", cult, nrow(sub)))
  cat(sprintf("         recolte_kg : Min=%9.1f  Médiane=%9.1f  Max=%9.1f  NA=%d\n",
              min(sub$recolte_kg, na.rm=TRUE),
              median(sub$recolte_kg, na.rm=TRUE),
              max(sub$recolte_kg, na.rm=TRUE),
              sum(is.na(sub$recolte_kg))))
  cat("         Unités UML utilisées : ")
  print(table(sub$uml_recolte))
  cat("\n")
}
cat("Décision : les quantités converties sont utilisées pour le calcul de la production.\n")
cat("          Les valeurs extrêmes (hors 5e-95e percentiles) sont winsorisées dans les rendements.\n\n")

# --- 4. Ventes (s16d converti) ---
cat("4. Ventes (s16d_conv)\n")
cat("-----------------------\n")
for (cult in c("Mil","Sorgho","Riz","Maïs")) {
  code <- groupes[[cult]]$prod
  sub <- s16d_conv %>% filter(s16dq01 == code, vente_oui == 1 | revenu_vente > 0)
  cat(sprintf("  %-8s : %4d ménages vendeurs\n", cult, nrow(sub)))
  if (nrow(sub) > 0) {
    cat(sprintf("         vente_kg : Min=%9.1f  Médiane=%9.1f  Max=%9.1f\n",
                min(sub$vente_kg, na.rm=TRUE),
                median(sub$vente_kg, na.rm=TRUE),
                max(sub$vente_kg, na.rm=TRUE)))
    cat(sprintf("         revenu   : Min=%9.0f  Médiane=%9.0f  Max=%9.0f\n\n",
                min(sub$revenu_vente, na.rm=TRUE),
                median(sub$revenu_vente, na.rm=TRUE),
                max(sub$revenu_vente, na.rm=TRUE)))
  } else {
    cat("\n")
  }
}
cat("Décision : les indicateurs de commercialisation sont calculés sur les ménages ayant déclaré une vente.\n")
cat("          Le taux de commercialisation = quantité vendue / production totale.\n\n")

# --- 5. Consommation (s07b) ---
cat("5. Consommation (s07b)\n")
cat("------------------------\n")
conso_poids <- s07b_raw %>%
  left_join(tous_menages, by = "hhid") %>%
  left_join(s00_raw %>% select(hhid, region = s00q01, milieu = s00q04), by = "hhid")

cat("Prévalence brute (non pondérée) par céréale :\n")
for (cult in names(groupes)) {
  codes <- groupes[[cult]]$conso
  n_conso <- conso_poids %>%
    filter(s07bq01 %in% codes, s07bq02 == 1) %>%
    distinct(hhid) %>% nrow()
  cat(sprintf("  %-8s : %d ménages\n", cult, n_conso))
}

cat("\nQuantités déclarées (UML) pour les produits consommés :\n")
for (cult in names(groupes)) {
  codes <- groupes[[cult]]$conso
  sub <- conso_poids %>% filter(s07bq01 %in% codes, s07bq02 == 1)
  cat(sprintf("  %-8s : %5d lignes,  min=%5.2f  médiane=%5.2f  max=%5.2f\n",
              cult, nrow(sub),
              min(sub$s07bq03a, na.rm=TRUE),
              median(sub$s07bq03a, na.rm=TRUE),
              max(sub$s07bq03a, na.rm=TRUE)))
  pct_kg <- mean(sub$s07bq03b == 100, na.rm=TRUE) * 100
  cat(sprintf("         Part en kg : %.1f%%\n", pct_kg))
}
cat("\nDécision : la conversion NSU (table EHCVM) est utilisée pour les quantités consommées.\n")
cat("          Les calories sont ajustées pour la partie non comestible (refuse).\n\n")

cat("============================================================\n")
cat("DÉCISIONS FINALES\n")
cat("============================================================\n")
cat("- Produit retenu pour l'étude de filière : le mil.\n")
cat("  Justification : il domine en superficie et en nombre de producteurs,\n")
cat("  c'est une culture vivrière essentielle sans dépendance aux importations.\n")
cat("- Comparaison élargie à 4 céréales pour le cadrage.\n")
cat("- Superficies : culture principale de la parcelle, GPS prioritaire.\n")
cat("- Conversion des unités de production : table externe EAC (Charretée, Bassine,\n")
cat("  Panier, Gerbe) avec facteurs par produit × région × milieu.\n")
cat("- Conversion des unités de consommation : table NSU standard EHCVM.\n")
cat("- Indicateurs pondérés par les poids d'enquête (hhweight).\n")
cat("- Rendements winsorisés à 1% et 99%.\n")
cat("- FAOSTAT : production et commerce via fichiers Excel (.xls) ; disponibilité et\n")
cat("  calories via le fichier RDS issu des bilans alimentaires.\n")
cat("- Balance commerciale riz : calculée sur l'item 'Riz, paddy (riz blanchi\n")
cat("  équivalent)', qui agrège les importations de riz usiné/brisures/décortiqué\n")
cat("  reconverties en équivalent-paddy. L'item 'Riz' seul ne couvre que le paddy\n")
cat("  brut et sous-estime massivement la dépendance aux importations.\n")
cat("============================================================\n\n")

sink()

# ------------------------------------------------------------------------------
# 3. Fonctions pondérées
# ------------------------------------------------------------------------------

total_pondere  <- function(x, w) sum(x * w, na.rm = TRUE)
moyenne_ponderee <- function(x, w) weighted.mean(x, w, na.rm = TRUE)

# ------------------------------------------------------------------------------
# 4. Indicateurs de consommation
# ------------------------------------------------------------------------------

cat("Calcul des indicateurs de consommation...\n")

# 4.1 Prévalence
conso_poids <- s07b_raw %>%
  left_join(tous_menages, by = "hhid") %>%
  left_join(s00_raw %>% select(hhid, region = s00q01, milieu = s00q04), by = "hhid")

prevalence <- list()
for (nom in names(groupes)) {
  codes <- groupes[[nom]]$conso
  conso_groupe <- conso_poids %>%
    filter(s07bq01 %in% codes, s07bq02 == 1) %>%
    distinct(hhid, hhweight)
  prevalence[[nom]] <- total_pondere(rep(1, nrow(conso_groupe)), conso_groupe$hhweight) /
    total_pondere(rep(1, nrow(tous_menages)), tous_menages$hhweight)
}

# 4.2 Part budgétaire
dep_alim <- conso_agr %>%
  filter(coicop == 1) %>%
  group_by(grappe, menage, vague) %>%
  summarise(dep_tot = sum(depan, na.rm = TRUE), .groups = "drop") %>%
  left_join(s00_raw %>% select(grappe, menage, vague, hhid), by = c("grappe", "menage", "vague")) %>%
  left_join(poids_menages, by = "hhid")

total_dep_alim <- total_pondere(dep_alim$dep_tot, dep_alim$hhweight)

part_budget <- list()
for (nom in names(groupes)) {
  codes <- groupes[[nom]]$conso
  dep_prod <- conso_agr %>%
    filter(codpr %in% codes) %>%
    group_by(grappe, menage, vague) %>%
    summarise(dep_prod = sum(depan, na.rm = TRUE), .groups = "drop") %>%
    left_join(s00_raw %>% select(grappe, menage, vague, hhid), by = c("grappe", "menage", "vague")) %>%
    left_join(poids_menages, by = "hhid")
  part_budget[[nom]] <- total_pondere(dep_prod$dep_prod, dep_prod$hhweight) / total_dep_alim
}

# 4.3 Part en poids et en calories (conversion NSU)
nsu_fallback <- nsu_raw %>%
  group_by(produitID, uniteID, tailleID) %>%
  summarise(poids_fallback = median(poids, na.rm = TRUE), .groups = "drop")

conso_kg <- conso_poids %>%
  filter(s07bq02 == 1) %>%
  mutate(qte_kg = if_else(s07bq03b == 100, s07bq03a, NA_real_),
         strate_menage = region * 10 + milieu)

conso_nsu <- suppressWarnings({
  conso_kg %>%
    filter(s07bq03b != 100, s07bq03b != 101) %>%
    left_join(nsu_raw, by = c("s07bq01" = "produitID", "s07bq03b" = "uniteID",
                              "s07bq03c" = "tailleID", "strate_menage" = "strate")) %>%
    left_join(nsu_fallback, by = c("s07bq01" = "produitID", "s07bq03b" = "uniteID",
                                   "s07bq03c" = "tailleID")) %>%
    mutate(poids_utilise = if_else(!is.na(poids), poids, poids_fallback),
           qte_kg_nsu = s07bq03a * poids_utilise / 1000)
})

conso_finale <- bind_rows(
  conso_kg %>% filter(s07bq03b == 100) %>% mutate(qte_kg_final = qte_kg),
  conso_nsu %>% filter(!is.na(qte_kg_nsu)) %>% mutate(qte_kg_final = qte_kg_nsu)
)

cal <- load_dta(paths$raw_auxiliaires, "calorie_conversion_wa_2021.dta") %>%
  select(codpr, cal, refuse)
conso_finale <- suppressWarnings(
  conso_finale %>% left_join(cal, by = c("s07bq01" = "codpr"))
)

poids_cereales <- list()
calories_cereales <- list()

total_poids_alim <- conso_finale %>%
  mutate(poids_pond = qte_kg_final * hhweight) %>%
  pull(poids_pond) %>% sum(na.rm = TRUE)

total_cal_alim <- conso_finale %>%
  mutate(cal_pond = qte_kg_final * 10 * cal * (1 - refuse/100) * hhweight) %>%
  pull(cal_pond) %>% sum(na.rm = TRUE)

for (nom in names(groupes)) {
  codes <- groupes[[nom]]$conso
  temp <- conso_finale %>% filter(s07bq01 %in% codes)
  
  poids_cereales[[nom]] <- temp %>%
    mutate(p = qte_kg_final * hhweight) %>%
    pull(p) %>% sum(na.rm = TRUE)
  
  calories_cereales[[nom]] <- temp %>%
    mutate(cal = qte_kg_final * 10 * cal * (1 - refuse/100) * hhweight) %>%
    pull(cal) %>% sum(na.rm = TRUE)
}

part_poids_cereales <- lapply(poids_cereales, function(x) x / sum(unlist(poids_cereales)))
part_cal_cereales  <- lapply(calories_cereales, function(x) x / sum(unlist(calories_cereales)))

# ------------------------------------------------------------------------------
# 5. Indicateurs de production
# ------------------------------------------------------------------------------

cat("Calcul des indicateurs de production...\n")

# 5.1 Superficie (culture principale de la parcelle)
superficie_parc <- s16a_raw %>%
  filter(s16aq08 %in% c(1,2,3,4)) %>%
  mutate(
    sup_ha = case_when(
      !is.na(s16aq47) & s16aq47 > 0 ~ s16aq47,
      s16aq09b == 1 ~ s16aq09a,
      s16aq09b == 2 ~ s16aq09a / 10000,
      TRUE ~ NA_real_
    )
  )

sup_menage <- superficie_parc %>%
  group_by(hhid, culture = s16aq08) %>%
  summarise(superficie = sum(sup_ha, na.rm = TRUE), .groups = "drop") %>%
  left_join(poids_menages, by = "hhid")

taux_prod <- list()
superficie_tot <- list()

for (nom in names(groupes)) {
  code <- groupes[[nom]]$prod
  temp <- sup_menage %>% filter(culture == code)
  menages_prod <- temp %>% distinct(hhid, hhweight)
  taux_prod[[nom]] <- total_pondere(rep(1, nrow(menages_prod)), menages_prod$hhweight) /
    total_pondere(rep(1, nrow(tous_menages)), tous_menages$hhweight)
  superficie_tot[[nom]] <- total_pondere(temp$superficie, temp$hhweight)
}

# 5.2 Production totale (kg) via s16d converti
prod_menage <- s16d_conv %>%
  mutate(
    prod_totale_kg = coalesce(conso_kg,0) + coalesce(don_kg,0) +
      coalesce(vente_kg,0) + coalesce(stock_kg,0)
  ) %>%
  left_join(poids_menages, by = "hhid")

prod_kg <- list()
rendement <- list()

for (nom in names(groupes)) {
  code <- groupes[[nom]]$prod
  temp <- prod_menage %>% filter(s16dq01 == code)
  
  prod_menage_agg <- temp %>%
    group_by(hhid, hhweight) %>%
    summarise(production_kg = sum(prod_totale_kg, na.rm = TRUE), .groups = "drop")
  
  prod_kg[[nom]] <- total_pondere(prod_menage_agg$production_kg, prod_menage_agg$hhweight) / 1000
  
  sup_temp <- sup_menage %>% filter(culture == code)
  rend_join <- sup_temp %>%
    left_join(prod_menage_agg, by = c("hhid", "hhweight")) %>%
    filter(superficie > 0, production_kg > 0) %>%
    mutate(rdt = production_kg / superficie,
           rdt_w = winsorize(rdt, probs = c(0.01, 0.99)))
  
  if (nrow(rend_join) > 0) {
    rendement[[nom]] <- moyenne_ponderee(rend_join$rdt_w, rend_join$hhweight)
  } else {
    rendement[[nom]] <- NA_real_
  }
}

# ------------------------------------------------------------------------------
# 6. Commercialisation
# ------------------------------------------------------------------------------

cat("Calcul des indicateurs de commercialisation...\n")

valeur_ventes <- list()
taux_com <- list()
prix_imp <- list()

for (nom in names(groupes)) {
  code <- groupes[[nom]]$prod
  
  ventes <- s16d_conv %>%
    filter(s16dq01 == code, (vente_oui == 1 | revenu_vente > 0)) %>%
    left_join(poids_menages, by = "hhid")
  
  if (nrow(ventes) == 0) {
    valeur_ventes[[nom]] <- 0
    taux_com[[nom]] <- 0
    prix_imp[[nom]] <- NA
    next
  }
  
  ventes_menage <- ventes %>%
    group_by(hhid, hhweight) %>%
    summarise(revenu = sum(revenu_vente, na.rm = TRUE), .groups = "drop")
  valeur_ventes[[nom]] <- total_pondere(ventes_menage$revenu, ventes_menage$hhweight)
  
  ventes_kg <- ventes %>%
    filter(!is.na(vente_kg) & vente_kg > 0) %>%
    group_by(hhid, hhweight) %>%
    summarise(qte_vendue_kg = sum(vente_kg, na.rm = TRUE), .groups = "drop")
  
  qte_totale <- total_pondere(ventes_kg$qte_vendue_kg, ventes_kg$hhweight)
  prod_totale_kg <- prod_kg[[nom]] * 1000
  taux_com[[nom]] <- qte_totale / prod_totale_kg
  prix_imp[[nom]] <- valeur_ventes[[nom]] / qte_totale
}

# ------------------------------------------------------------------------------
# 7. Données macro FAOSTAT
# ------------------------------------------------------------------------------

cat("Extraction FAOSTAT...\n")

fao_produit_prod <- c(
  "Mil"    = "Mils",
  "Sorgho" = "Sorgho",
  "Riz"    = "Riz",
  "Maïs"   = "Maïs"
)

fao_produit_trade <- c(
  "Mil"    = "Mils",
  "Sorgho" = "Sorgho",
  "Riz"    = "Riz, paddy (riz blanchi équivalent)",
  "Maïs"   = "Maïs"
)

macro_prod <- list()
balance   <- list()

for (nom in names(groupes)) {
  ligne_prod <- faostat_prod %>%
    filter(Produit == fao_produit_prod[[nom]], Année == 2021, Élément == "Production")
  macro_prod[[nom]] <- if (nrow(ligne_prod) > 0) ligne_prod$Valeur[1] else NA_real_
  
  ligne_trade <- faostat_trade %>%
    filter(Produit == fao_produit_trade[[nom]], Année == 2021)
  
  import <- sum(ligne_trade$Valeur[ligne_trade$Élément == "Importations - quantité"], na.rm = TRUE)
  export <- sum(ligne_trade$Valeur[ligne_trade$Élément == "Exportations - quantité"], na.rm = TRUE)
  
  balance[[nom]] <- if (nrow(ligne_trade) > 0) import - export else NA_real_
}

faostat_mali <- readRDS(here::here("data", "raw", "faostat" , "faostat_mali.rds"))
dispo_vec <- setNames(faostat_mali$dispo_kg$kg_capita_an, faostat_mali$dispo_kg$produit)
cal_vec   <- setNames(faostat_mali$parts_2021$kcal, faostat_mali$parts_2021$produit)

# ------------------------------------------------------------------------------
# 8. Synthèse et export Excel
# ------------------------------------------------------------------------------

synthese <- data.frame(
  Produit                = names(groupes),
  Prev_conso             = round(unlist(prevalence) * 100, 1),
  Part_budget            = round(unlist(part_budget) * 100, 1),
  Part_poids_cereales    = round(unlist(part_poids_cereales) * 100, 1),
  Part_calories_cereales = round(unlist(part_cal_cereales) * 100, 1),
  Prev_prod              = round(unlist(taux_prod) * 100, 1),
  Superficie_ha          = round(unlist(superficie_tot)),
  Production_tonnes      = round(unlist(prod_kg)),
  Rendement_kg_ha        = round(unlist(rendement)),
  Valeur_ventes          = round(unlist(valeur_ventes)),
  Tx_commercialisation   = round(unlist(taux_com) * 100, 1),
  Prix_FCFA_kg           = round(unlist(prix_imp)),
  Prod_FAO_tonnes        = round(unlist(macro_prod)),
  Balance_com_tonnes     = round(unlist(balance), 1),
  Dispo_kg_capita_an     = round(dispo_vec[names(groupes)], 1),
  Kcal_capita_jour       = round(cal_vec[names(groupes)], 0),
  stringsAsFactors = FALSE
)

print(synthese)

wb <- createWorkbook()
addWorksheet(wb, "Comparaison produits")
addWorksheet(wb, "Dictionnaire")

header_style <- createStyle(fontColour = "#ffffff", fgFill = "#4B5320",
                            halign = "center", textDecoration = "bold",
                            border = "TopBottomLeftRight")
body_style   <- createStyle(halign = "left", border = "TopBottomLeftRight")
title_style  <- createStyle(fontSize = 14, fontColour = "#2E4053", textDecoration = "bold")

writeData(wb, "Comparaison produits",
          "Indicateurs comparés – Mil, Sorgho, Riz, Maïs (EHCVM Mali 2021/2022)",
          startCol = 1, startRow = 1)
mergeCells(wb, "Comparaison produits", cols = 1:ncol(synthese), rows = 1)
addStyle(wb, "Comparaison produits", title_style, rows = 1, cols = 1:ncol(synthese), gridExpand = TRUE)

writeData(wb, "Comparaison produits", synthese, startRow = 3)
addStyle(wb, "Comparaison produits", header_style, rows = 3, cols = 1:ncol(synthese), gridExpand = TRUE)
addStyle(wb, "Comparaison produits", body_style, rows = 4:(4 + nrow(synthese) - 1), cols = 1:ncol(synthese), gridExpand = TRUE)
setColWidths(wb, "Comparaison produits", cols = 1:ncol(synthese), widths = "auto")

dico_indic <- data.frame(
  Indicateur = names(synthese)[-1],
  Description = c(
    "Part des ménages consommateurs (%)",
    "Part dans la dépense alimentaire totale (%)",
    "Part dans le poids total des 4 céréales (%)",
    "Part dans les calories des 4 céréales (%)",
    "Part des ménages producteurs (%)",
    "Superficie totale cultivée (ha)",
    "Production totale estimée (tonnes)",
    "Rendement moyen winsorisé (kg/ha)",
    "Valeur totale des ventes (FCFA)",
    "Taux de commercialisation (%)",
    "Prix implicite producteur (FCFA/kg)",
    "Production nationale FAO (tonnes)",
    "Balance commerciale FAO (tonnes) - riz en équivalent paddy",
    "Disponibilité FAO (kg/hab/an)",
    "Calories FAO (kcal/hab/jour)"
  )
)
writeData(wb, "Dictionnaire", dico_indic, startRow = 1)
addStyle(wb, "Dictionnaire", header_style, rows = 1, cols = 1:2, gridExpand = TRUE)
addStyle(wb, "Dictionnaire", body_style, rows = 2:(2 + nrow(dico_indic) - 1), cols = 1:2, gridExpand = TRUE)
setColWidths(wb, "Dictionnaire", cols = 1:2, widths = c(30, 60))

saveWorkbook(wb, here::here("outputs", "tables", "synthese_produits.xlsx"), overwrite = TRUE)

# ------------------------------------------------------------------------------
# 9. Visualisations
# ------------------------------------------------------------------------------

theme_filiere <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14, hjust = 0.5, color = "#2E4053"),
    plot.subtitle    = element_text(hjust = 0.5, color = "#5D6D7E"),
    plot.caption     = element_text(color = "#95A5A6", size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#D5D8DC"),
    legend.position  = "bottom",
    legend.title     = element_text(size = 10),
    axis.title       = element_text(size = 11, color = "#2C3E50"),
    axis.text        = element_text(size = 9, color = "#2C3E50")
  )

construire_waffle <- function(cereale_nom, part_pct) {
  n_remplies <- round(part_pct)
  statut <- c(rep("cultive", n_remplies), rep("non", 100 - n_remplies))
  tibble(
    cereale = cereale_nom,
    case    = 1:100,
    statut  = statut,
    x = rep(1:10, times = 10),
    y = rep(1:10, each  = 10)
  )
}

donnees_waffle <- purrr::pmap_dfr(
  list(names(groupes), unlist(taux_prod) * 100),
  construire_waffle
)

ordre_cereales <- names(sort(unlist(taux_prod), decreasing = TRUE))
donnees_waffle <- donnees_waffle %>%
  mutate(cereale = factor(cereale, levels = ordre_cereales))

etiquettes <- tibble(
  cereale = ordre_cereales,
  lab = paste0(cereale, " : ", round(unlist(taux_prod)[ordre_cereales] * 100, 1), " %")
) %>% mutate(lab = factor(lab, levels = lab))

donnees_waffle <- donnees_waffle %>% left_join(etiquettes, by = "cereale")

g_waffle <- ggplot(donnees_waffle, aes(x = x, y = y, fill = statut)) +
  geom_tile(color = "white", linewidth = 0.8) +
  facet_wrap(~ lab, nrow = 1) +
  coord_equal() +
  scale_fill_manual(values = c("cultive" = "#2c6e49", "non" = "#e9ecef")) +
  labs(title = "Sur 100 ménages agricoles, combien cultivent chaque céréale ?",
       subtitle = "Culture principale de la parcelle",
       caption = "Source : EHCVM-2 Mali 2021/22. Pondération nationale.") +
  theme_filiere +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), legend.position = "bottom")

ggsave(file.path(paths$figures, "axe2_waffle_producteurs.png"),
       g_waffle, width = 12, height = 4, dpi = 300, bg = "white")

prep_superficie <- data.frame(
  cereale = names(groupes),
  superf_millions_ha = unlist(superficie_tot) / 1e6
) %>%
  mutate(cereale = factor(cereale, levels = cereale[order(superf_millions_ha)]),
         est_mil = if_else(cereale == "Mil", "Mil", "Autres"))

g_superficie <- ggplot(prep_superficie, aes(x = cereale, y = superf_millions_ha, fill = est_mil)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(round(superf_millions_ha, 2), " M ha")),
            hjust = -0.15, size = 4, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("Mil" = "#2c6e49", "Autres" = "#a3b18a")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Le mil occupe la plus grande superficie cultivée",
       subtitle = "Superficie totale par céréale (extrapolation nationale)",
       x = NULL, y = "Superficie (millions d'hectares)",
       caption = "Source : EHCVM-2 Mali 2021/22. GPS prioritaire.") +
  theme_filiere + theme(legend.position = "none")

ggsave(file.path(paths$figures, "axe2_superficie.png"),
       g_superficie, width = 8, height = 5, dpi = 300, bg = "white")

d_prev <- data.frame(cereale = names(groupes), valeur = unlist(prevalence) * 100,
                     mesure = "Prévalence (% des ménages)")
d_poids <- data.frame(cereale = names(groupes), valeur = unlist(part_poids_cereales) * 100,
                      mesure = "Poids (% des céréales)")
d_cal <- data.frame(cereale = names(groupes), valeur = unlist(part_cal_cereales) * 100,
                    mesure = "Calories (% des céréales)")

donnees_cons <- bind_rows(d_prev, d_poids, d_cal) %>%
  mutate(mesure = factor(mesure, levels = c("Prévalence (% des ménages)",
                                            "Poids (% des céréales)",
                                            "Calories (% des céréales)")))

ordre_prev <- names(sort(unlist(prevalence), decreasing = TRUE))
donnees_cons <- donnees_cons %>%
  mutate(cereale = factor(cereale, levels = rev(ordre_prev)),
         est_riz = if_else(cereale == "Riz", "Riz", "Autres"))

g_cons <- ggplot(donnees_cons, aes(x = cereale, y = valeur, fill = est_riz)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(round(valeur, 1), " %")), vjust = -0.4, size = 3.2) +
  facet_wrap(~ mesure, scales = "free_y") +
  scale_fill_manual(values = c("Riz" = "#1d3557", "Autres" = "#a8dadc")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Le riz domine la consommation, le mil domine la production",
       subtitle = "Consommation des ménages (EHCVM-2 2021/22)",
       x = NULL, y = NULL) +
  theme_filiere + theme(legend.position = "none")

ggsave(file.path(paths$figures, "axe1_synthese_consommation.png"),
       g_cons, width = 12, height = 4.5, dpi = 300, bg = "white")

cat("\nÉtape 0 terminée.\n")