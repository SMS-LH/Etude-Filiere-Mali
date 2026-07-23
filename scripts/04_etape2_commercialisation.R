# ==============================================================================
# 04_etape2_commercialisation.R
# Étape 2 – Commercialisation et prix du mil
# EDA, indicateurs, prix régionaux, marges, modèles, cartes (template ggplot2)
# ==============================================================================

source(here::here("scripts", "00_setup.R"))

create_dir(here::here("outputs", "eda"))

# ------------------------------------------------------------------------------
# 1. Chargement des données
# ------------------------------------------------------------------------------

s00_raw  <- load_dta(paths$raw_menage, "s00_me_mli2021.dta")
s07b_raw <- load_dta(paths$raw_menage, "s07b_me_mli2021.dta")
s16d_raw <- load_dta(paths$raw_menage, "s16d_me_mli2021.dta")
prix_raw <- load_dta(paths$raw_auxiliaires, "ehcvm_prix_mli2021.dta")
pond     <- load_dta(paths$raw_auxiliaires, "ehcvm_ponderations_mli2021.dta")
nsu_raw  <- load_dta(paths$raw_auxiliaires, "ehcvm_nsu_mli2021.dta")

s16d_conv <- load_rds(paths$processed, "s16d_converti.rds")
if (!"hhid" %in% names(s16d_conv)) {
  s16d_conv <- creer_hhid(s16d_conv)
}

comm1 <- load_dta(paths$raw_communaute, "s01_co_mli2021.dta")
comm2 <- load_dta(paths$raw_communaute, "s02_co_mli2021.dta")
comm3 <- load_dta(paths$raw_communaute, "s03_co_mli2021.dta")

mali_regions <- st_read(here::here("data", "raw", "spatial", "gadm", "gadm41_MLI_1.shp"), quiet = TRUE)

# ------------------------------------------------------------------------------
# 2. Identifiants, poids, géolocalisation
# ------------------------------------------------------------------------------

creer_hhid <- function(df) df %>% mutate(hhid = paste(grappe, menage, vague, sep = "_"))

s00_raw  <- creer_hhid(s00_raw)
s07b_raw <- creer_hhid(s07b_raw)
s16d_raw <- creer_hhid(s16d_raw)

poids_menages <- s00_raw %>%
  select(grappe, menage, vague, hhid) %>%
  left_join(pond, by = c("grappe", "menage")) %>%
  select(hhid, hhweight)

region_info <- s00_raw %>%
  select(hhid, grappe, vague, region = s00q01, milieu = s00q04)

# ------------------------------------------------------------------------------
# 3. Variables communautaires
# ------------------------------------------------------------------------------

comm_vars <- comm1 %>%
  select(grappe, vague, dist_ville = s01q05, type_voie = s01q06) %>%
  mutate(type_voie = case_when(
    type_voie == 1 ~ "Goudronnée",
    type_voie == 2 ~ "Latérite",
    type_voie == 3 ~ "Piste",
    TRUE ~ "Autre"
  ))

temps_marche <- comm2 %>%
  filter(s02q00 == 10) %>%
  group_by(grappe, vague) %>%
  summarise(temps_marche = if (all(is.na(s02q03))) NA_real_ else min(s02q03, na.rm = TRUE),
            .groups = "drop")

cooperative <- comm3 %>%
  mutate(grappe = as.integer(grappe),
         cooperative = as.integer(as.integer(s03q03) == 1)) %>%
  select(grappe, vague, cooperative)

comm_vars <- comm_vars %>%
  left_join(temps_marche, by = c("grappe", "vague")) %>%
  left_join(cooperative, by = c("grappe", "vague"))

# ==============================================================================
# PHASE 1 – EXPLORATION DES DONNÉES (EDA)
# ==============================================================================

sink(here::here("outputs", "eda", "eda_etape2.txt"), split = TRUE)
cat("============================================================\n")
cat("EDA – Étape 2 : Commercialisation et prix du mil\n")
cat("Date :", format(Sys.time()), "\n")
cat("============================================================\n\n")

cat("1. Ventes de mil (s16d_conv)\n")
cat("-------------------------------\n")

ventes_eda <- s16d_conv %>%
  filter(s16dq01 == 1, (vente_oui == 1 | revenu_vente > 0))

cat(sprintf("Nombre de ménages vendeurs : %d\n", nrow(ventes_eda)))
cat(sprintf("Quantité vendue (kg) : min=%.1f  médiane=%.1f  max=%.1f\n",
            min(ventes_eda$vente_kg, na.rm = TRUE),
            median(ventes_eda$vente_kg, na.rm = TRUE),
            max(ventes_eda$vente_kg, na.rm = TRUE)))
cat(sprintf("Revenu (FCFA) : min=%.0f  médiane=%.0f  max=%.0f\n",
            min(ventes_eda$revenu_vente, na.rm = TRUE),
            median(ventes_eda$revenu_vente, na.rm = TRUE),
            max(ventes_eda$revenu_vente, na.rm = TRUE)))

prix_prod_eda <- ventes_eda %>%
  mutate(prix_kg = revenu_vente / vente_kg) %>%
  filter(prix_kg >= 50, prix_kg <= 1000)

cat(sprintf("Prix producteur après bornes 50-1000 FCFA/kg : %d obs.\n", nrow(prix_prod_eda)))
cat(sprintf("  Min=%.1f  Q1=%.1f  Médiane=%.1f  Q3=%.1f  Max=%.1f\n",
            min(prix_prod_eda$prix_kg), quantile(prix_prod_eda$prix_kg, 0.25),
            median(prix_prod_eda$prix_kg), quantile(prix_prod_eda$prix_kg, 0.75),
            max(prix_prod_eda$prix_kg)))

canal_eda <- s16d_raw %>%
  filter(s16dq01 == 1, (s16dq04 == 1 | s16dq06 > 0)) %>%
  select(hhid, s16dq08)

cat("\nCanaux de vente :\n")
print(table(canal_eda$s16dq08, useNA = "ifany"))

cat("\n2. Prix de marché (ehcvm_prix)\n")
cat("--------------------------------\n")

prix_marche_eda <- prix_raw %>%
  filter(produitID == 7) %>%
  mutate(prix_kg_corrige = (prix / (poids / 1000)) / 1000)

cat(sprintf("Nombre de relevés : %d\n", nrow(prix_marche_eda)))
cat(sprintf("Prix corrigé (FCFA/kg) : min=%.0f  médiane=%.0f  max=%.0f\n",
            min(prix_marche_eda$prix_kg_corrige),
            median(prix_marche_eda$prix_kg_corrige),
            max(prix_marche_eda$prix_kg_corrige)))

cat("\n3. Prix à la consommation (s07b)\n")
cat("-----------------------------------\n")

nsu_fallback <- nsu_raw %>%
  group_by(produitID, uniteID, tailleID) %>%
  summarise(poids_fallback = median(poids, na.rm = TRUE), .groups = "drop")

achats_eda <- s07b_raw %>%
  filter(s07bq01 %in% c(7, 14, 15), s07bq06 %in% c(1,2,3), s07bq07a > 0, s07bq08 > 0) %>%
  left_join(region_info, by = "hhid") %>%
  mutate(strate_menage = region * 10 + milieu)

achats_kg <- achats_eda %>%
  mutate(qte_kg = if_else(s07bq07b == 100, s07bq07a, NA_real_))

achats_nsu <- suppressWarnings({
  achats_kg %>%
    filter(s07bq07b != 100, s07bq07b != 101) %>%
    left_join(nsu_raw, by = c("s07bq01" = "produitID", "s07bq07b" = "uniteID",
                              "s07bq07c" = "tailleID", "strate_menage" = "strate")) %>%
    left_join(nsu_fallback, by = c("s07bq01" = "produitID", "s07bq07b" = "uniteID",
                                   "s07bq07c" = "tailleID")) %>%
    mutate(qte_kg_nsu = s07bq07a * poids_fallback / 1000)
})

achats_final_eda <- bind_rows(
  achats_kg %>% filter(s07bq07b == 100) %>% mutate(qte_kg_final = qte_kg),
  achats_nsu %>% filter(!is.na(qte_kg_nsu)) %>% mutate(qte_kg_final = qte_kg_nsu)
) %>%
  mutate(prix_conso = s07bq08 / qte_kg_final) %>%
  filter(prix_conso >= 50, prix_conso <= 1000)

cat(sprintf("Achats récents retenus : %d lignes\n", nrow(achats_final_eda)))
cat(sprintf("Prix conso (FCFA/kg) : min=%.1f  Q1=%.1f  Médiane=%.1f  Q3=%.1f  Max=%.1f\n",
            min(achats_final_eda$prix_conso), quantile(achats_final_eda$prix_conso, 0.25),
            median(achats_final_eda$prix_conso), quantile(achats_final_eda$prix_conso, 0.75),
            max(achats_final_eda$prix_conso)))

cat("\nEDA terminée.\n")
sink()

# ==============================================================================
# PHASE 2 – INDICATEURS DE COMMERCIALISATION
# ==============================================================================

# --- Production totale ---
prod_menage <- s16d_conv %>%
  filter(s16dq01 == 1) %>%
  mutate(prod_totale_kg = coalesce(conso_kg,0) + coalesce(don_kg,0) +
           coalesce(vente_kg,0) + coalesce(stock_kg,0)) %>%
  group_by(hhid) %>%
  summarise(production_kg = sum(prod_totale_kg, na.rm = TRUE), .groups = "drop")

# --- Ventes nettoyées ---
ventes <- s16d_conv %>%
  filter(s16dq01 == 1, vente_oui == 1 | revenu_vente > 0) %>%
  select(-any_of(c("grappe", "vague", "region", "milieu"))) %>%
  mutate(
    prix_kg = revenu_vente / vente_kg,
    prix_prod_borne = if_else(prix_kg >= 50 & prix_kg <= 1000, prix_kg, NA_real_)
  ) %>%
  filter(!is.na(prix_prod_borne)) %>%
  mutate(prix_prod_w = winsorize(prix_prod_borne, probs = c(0.01, 0.99))) %>%
  left_join(poids_menages, by = "hhid") %>%
  left_join(region_info, by = "hhid") %>%
  left_join(canal_eda, by = "hhid") %>%
  left_join(comm_vars, by = c("grappe", "vague"))

# --- Taux de commercialisation ---
commercialisation <- prod_menage %>%
  left_join(ventes %>% select(hhid, vente_kg, revenu_vente, prix_prod_w), by = "hhid") %>%
  left_join(poids_menages, by = "hhid") %>%
  mutate(
    vente_kg = if_else(is.na(vente_kg), 0, vente_kg),
    prod_totale_kg = production_kg
  )

qte_totale_vendue   <- sum(commercialisation$vente_kg * commercialisation$hhweight, na.rm = TRUE)
qte_totale_produite <- sum(commercialisation$prod_totale_kg * commercialisation$hhweight, na.rm = TRUE)
taux_com_global <- qte_totale_vendue / qte_totale_produite

nb_producteurs <- nrow(prod_menage)
nb_vendeurs     <- nrow(ventes)
prop_vendeurs   <- nb_vendeurs / nb_producteurs

prix_prod_moyen <- weighted.mean(ventes$prix_prod_w, ventes$vente_kg * ventes$hhweight, na.rm = TRUE)
prix_prod_median <- median(ventes$prix_prod_w, na.rm = TRUE)

prix_prod_region <- ventes %>%
  group_by(region) %>%
  summarise(
    prix_moyen = weighted.mean(prix_prod_w, vente_kg * hhweight, na.rm = TRUE),
    nb_vendeurs = n(),
    .groups = "drop"
  )

# --- Prix à la consommation ---
achats <- s07b_raw %>%
  filter(s07bq01 %in% c(7, 14, 15), s07bq06 %in% c(1,2,3), s07bq07a > 0, s07bq08 > 0) %>%
  select(-any_of(c("grappe", "vague", "region", "milieu"))) %>%
  left_join(region_info, by = "hhid") %>%
  mutate(strate_menage = region * 10 + milieu)

achats_kg <- achats %>%
  mutate(qte_kg = if_else(s07bq07b == 100, s07bq07a, NA_real_))

achats_nsu <- suppressWarnings({
  achats_kg %>%
    filter(s07bq07b != 100, s07bq07b != 101) %>%
    left_join(nsu_raw, by = c("s07bq01" = "produitID", "s07bq07b" = "uniteID",
                              "s07bq07c" = "tailleID", "strate_menage" = "strate")) %>%
    left_join(nsu_fallback, by = c("s07bq01" = "produitID", "s07bq07b" = "uniteID",
                                   "s07bq07c" = "tailleID")) %>%
    mutate(qte_kg_nsu = s07bq07a * poids_fallback / 1000)
})

achats_final <- bind_rows(
  achats_kg %>% filter(s07bq07b == 100) %>% mutate(qte_kg_final = qte_kg),
  achats_nsu %>% filter(!is.na(qte_kg_nsu)) %>% mutate(qte_kg_final = qte_kg_nsu)
) %>%
  mutate(prix_conso = s07bq08 / qte_kg_final) %>%
  filter(prix_conso >= 50, prix_conso <= 1000) %>%
  mutate(prix_conso_w = winsorize(prix_conso, probs = c(0.01, 0.99))) %>%
  left_join(poids_menages, by = "hhid")

prix_conso_moyen <- weighted.mean(achats_final$prix_conso_w,
                                  achats_final$qte_kg_final * achats_final$hhweight, na.rm = TRUE)
prix_conso_median <- median(achats_final$prix_conso_w, na.rm = TRUE)

prix_conso_region <- achats_final %>%
  group_by(region) %>%
  summarise(
    prix_moyen = weighted.mean(prix_conso_w, qte_kg_final * hhweight, na.rm = TRUE),
    n_acheteurs = n(),
    .groups = "drop"
  )

# --- Prix de marché corrigé ---
prix_marche <- prix_raw %>%
  filter(produitID == 7) %>%
  mutate(
    prix_kg_corrige = (prix / (poids / 1000)) / 1000,
    prix_kg_w = winsorize(prix_kg_corrige, probs = c(0.01, 0.99))
  )

prix_marche_moyen <- mean(prix_marche$prix_kg_w, na.rm = TRUE)
prix_marche_median <- median(prix_marche$prix_kg_w, na.rm = TRUE)

prix_marche_region <- prix_marche %>%
  group_by(region) %>%
  summarise(prix_moyen = mean(prix_kg_w, na.rm = TRUE), .groups = "drop")

# --- Marges nationales ---
marge_conso  <- (prix_conso_moyen  - prix_prod_moyen) / prix_prod_moyen * 100
marge_marche <- (prix_marche_moyen - prix_prod_moyen) / prix_prod_moyen * 100

# --- Prix et marges par région ---
prix_region <- prix_prod_region %>%
  full_join(prix_conso_region, by = "region") %>%
  left_join(prix_marche_region, by = "region") %>%
  rename(prix_prod = prix_moyen.x, prix_conso = prix_moyen.y,
         prix_marche = prix_moyen) %>%
  mutate(
    marge_conso_region = prix_conso - prix_prod,
    marge_marche_region = prix_marche - prix_prod
  )

# --- Modèle prix producteur ---
mod_prix <- feols(
  prix_prod_w ~ dist_ville + temps_marche + type_voie + cooperative +
    log(vente_kg + 1) + factor(region),
  data = ventes, weights = ~ hhweight, cluster = ~ grappe
)

coeffs <- coef(mod_prix)
ecarts <- se(mod_prix)
stats  <- coeffs / ecarts
signif <- ifelse(abs(stats) > 2.58, "***",
                 ifelse(abs(stats) > 1.96, "**",
                        ifelse(abs(stats) > 1.64, "*",
                               ifelse(abs(stats) > 1.28, ".", ""))))

tab_simple_prix <- data.frame(
  Variable     = names(coeffs),
  Coefficient  = paste0(round(coeffs, 3), signif),
  Ecart_type   = round(ecarts, 3),
  stringsAsFactors = FALSE
) %>%
  filter(Variable != "(Intercept)")

noms_renommes <- c(
  "dist_ville"               = "Distance ville (km)",
  "temps_marche"             = "Temps marché (min)",
  "type_voieLatérite"        = "Route latérite",
  "type_voiePiste"           = "Piste",
  "cooperative"              = "Coopérative",
  "log(vente_kg + 1)"        = "Log(quantité vendue + 1)",
  "factor(region)2"          = "Région Koulikoro",
  "factor(region)3"          = "Région Sikasso",
  "factor(region)4"          = "Région Ségou",
  "factor(region)5"          = "Région Mopti",
  "factor(region)6"          = "Région Tombouctou",
  "factor(region)7"          = "Région Gao",
  "factor(region)8"          = "Région Kidal",
  "factor(region)9"          = "Région Bamako"
)

tab_simple_prix <- tab_simple_prix %>%
  mutate(Variable = dplyr::recode(Variable, !!!noms_renommes))

print(tab_simple_prix)

tab_complet <- modelsummary(mod_prix, output = "data.frame", stars = TRUE)
saveRDS(tab_complet, here::here("data", "processed", "modele_prix_producteur.rds"))

# ==============================================================================
# EXPORT EXCEL UNIQUE
# ==============================================================================

wb <- createWorkbook()

titre_style  <- createStyle(fontSize = 14, fontColour = "#2E4053", textDecoration = "bold")
header_style <- createStyle(fontColour = "#ffffff", fgFill = "#4B5320",
                            halign = "center", textDecoration = "bold",
                            border = "TopBottomLeftRight")
body_style   <- createStyle(halign = "left", border = "TopBottomLeftRight")

# --- Feuille 1 : Indicateurs nationaux ---
indicateurs <- data.frame(
  Indicateur = c("Taux de commercialisation global",
                 "Part de producteurs vendeurs",
                 "Prix producteur moyen",
                 "Prix producteur médian",
                 "Prix consommation moyen",
                 "Prix consommation médian",
                 "Prix marché moyen corrigé",
                 "Prix marché médian corrigé",
                 "Marge producteur–consommateur",
                 "Marge producteur–marché"),
  Valeur = c(
    round(taux_com_global * 100, 1),
    round(prop_vendeurs * 100, 1),
    round(prix_prod_moyen, 0),
    round(prix_prod_median, 0),
    round(prix_conso_moyen, 0),
    round(prix_conso_median, 0),
    round(prix_marche_moyen, 0),
    round(prix_marche_median, 0),
    round(marge_conso, 1),
    round(marge_marche, 1)
  ),
  Unité = c("%", "%", "FCFA/kg", "FCFA/kg", "FCFA/kg", "FCFA/kg",
            "FCFA/kg", "FCFA/kg", "%", "%")
)

addWorksheet(wb, "Indicateurs nationaux")
writeData(wb, "Indicateurs nationaux", "Indicateurs de commercialisation du mil", startCol = 1, startRow = 1)
mergeCells(wb, "Indicateurs nationaux", cols = 1:3, rows = 1)
addStyle(wb, "Indicateurs nationaux", titre_style, rows = 1, cols = 1:3, gridExpand = TRUE)
writeData(wb, "Indicateurs nationaux", indicateurs, startRow = 3)
addStyle(wb, "Indicateurs nationaux", header_style, rows = 3, cols = 1:3, gridExpand = TRUE)
addStyle(wb, "Indicateurs nationaux", body_style, rows = 4:(4+nrow(indicateurs)-1), cols = 1:3, gridExpand = TRUE)
setColWidths(wb, "Indicateurs nationaux", cols = 1:3, widths = "auto")

# --- Feuille 2 : Prix et marges par région ---
prix_region_export <- prix_region %>%
  mutate(across(c(prix_prod, prix_conso, prix_marche, marge_conso_region, marge_marche_region),
                ~ round(., 0))) %>%
  rename(
    Région = region,
    `Prix producteur` = prix_prod,
    `Prix conso` = prix_conso,
    `Prix marché` = prix_marche,
    `Marge conso` = marge_conso_region,
    `Marge marché` = marge_marche_region
  )

addWorksheet(wb, "Prix et marges par région")
writeData(wb, "Prix et marges par région", "Prix (FCFA/kg) et marges (FCFA/kg) par région", startCol = 1, startRow = 1)
mergeCells(wb, "Prix et marges par région", cols = 1:7, rows = 1)
addStyle(wb, "Prix et marges par région", titre_style, rows = 1, cols = 1:7, gridExpand = TRUE)
writeData(wb, "Prix et marges par région", prix_region_export, startRow = 3)
addStyle(wb, "Prix et marges par région", header_style, rows = 3, cols = 1:7, gridExpand = TRUE)
addStyle(wb, "Prix et marges par région", body_style, rows = 4:(4+nrow(prix_region_export)-1), cols = 1:7, gridExpand = TRUE)
setColWidths(wb, "Prix et marges par région", cols = 1:7, widths = "auto")

# --- Feuille 3 : Modèles ---
addWorksheet(wb, "Modèles")
writeData(wb, "Modèles", "Déterminants du prix producteur du mil", startCol = 1, startRow = 1)
mergeCells(wb, "Modèles", cols = 1:3, rows = 1)
addStyle(wb, "Modèles", titre_style, rows = 1, cols = 1:3, gridExpand = TRUE)
writeData(wb, "Modèles", tab_simple_prix, startRow = 3)
addStyle(wb, "Modèles", header_style, rows = 3, cols = 1:3, gridExpand = TRUE)
addStyle(wb, "Modèles", body_style, rows = 4:(4+nrow(tab_simple_prix)-1), cols = 1:3, gridExpand = TRUE)
setColWidths(wb, "Modèles", cols = 1:3, widths = "auto")

# --- Feuille 4 : Dictionnaire ---
addWorksheet(wb, "Dictionnaire")
writeData(wb, "Dictionnaire", "Dictionnaire des indicateurs", startCol = 1, startRow = 1)
mergeCells(wb, "Dictionnaire", cols = 1:2, rows = 1)
addStyle(wb, "Dictionnaire", titre_style, rows = 1, cols = 1:2, gridExpand = TRUE)

dico <- data.frame(
  Indicateur = indicateurs$Indicateur,
  Description = c(
    "Rapport entre quantité totale vendue (pondérée) et production totale (pondérée), en %.",
    "Proportion de producteurs de mil ayant déclaré une vente, en %.",
    "Prix producteur moyen pondéré par quantités vendues et poids d'enquête (FCFA/kg).",
    "Prix producteur médian (FCFA/kg).",
    "Prix consommation moyen pondéré par quantités achetées et poids d'enquête (FCFA/kg).",
    "Prix consommation médian (FCFA/kg).",
    "Prix de marché moyen, corrigé de l'unité et winsorisé (FCFA/kg).",
    "Prix de marché médian corrigé (FCFA/kg).",
    "Écart relatif entre prix conso et prix producteur (%).",
    "Écart relatif entre prix marché et prix producteur (%)."
  )
)
writeData(wb, "Dictionnaire", dico, startRow = 3)
addStyle(wb, "Dictionnaire", header_style, rows = 3, cols = 1:2, gridExpand = TRUE)
addStyle(wb, "Dictionnaire", body_style, rows = 4:(4+nrow(dico)-1), cols = 1:2, gridExpand = TRUE)
setColWidths(wb, "Dictionnaire", cols = 1:2, widths = "auto")

saveWorkbook(wb, here::here("outputs", "tables", "resultats_etape2.xlsx"), overwrite = TRUE)

# ==============================================================================
# CARTOGRAPHIE (template ggplot2, 4 cartes)
# ==============================================================================

cat("\n--- Cartographie ---\n")

region_names <- c("1"="Kayes","2"="Koulikoro","3"="Sikasso","4"="Ségou",
                  "5"="Mopti","6"="Timbuktu","7"="Gao","8"="Kidal","9"="Bamako")

# Fonction générique avec conversion code -> nom de région
creer_carte <- function(data, colonne, titre_carte, legende, couleur_low = "#fee6ce", couleur_high = "#a63603") {
  # Ajouter le nom de la région à partir du code
  data <- data %>%
    mutate(NAME_1 = region_names[as.character(region)])
  
  carte_data <- mali_regions %>%
    select(NAME_1, geometry) %>%
    left_join(data, by = "NAME_1")
  
  centroides <- carte_data %>%
    st_centroid() %>%
    mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2],
           etiquette = ifelse(is.na(.[[colonne]]), NAME_1,
                              paste0(NAME_1, "\n", round(.[[colonne]], 0))))
  
  ggplot(carte_data) +
    geom_sf(aes_string(fill = colonne), color = "white", linewidth = 0.3) +
    scale_fill_gradient(low = couleur_low, high = couleur_high,
                        na.value = "grey90", name = legende) +
    geom_text(data = centroides, aes(x = lon, y = lat, label = etiquette),
              size = 3, fontface = "bold", lineheight = 0.9) +
    labs(title = titre_carte,
         subtitle = "Campagne 2021/22 – EHCVM-2 Mali",
         caption = "Source : EHCVM-2.") +
    theme_minimal() +
    theme(axis.text = element_blank(), axis.title = element_blank(),
          panel.grid = element_blank())
}

c1 <- creer_carte(prix_prod_region, "prix_moyen", "Prix producteur du mil par région", "FCFA/kg")
ggsave(file.path(paths$maps, "carte_prix_producteur_mil.png"), c1, width = 8, height = 7, dpi = 150, bg = "white")

c2 <- creer_carte(prix_conso_region, "prix_moyen", "Prix à la consommation du mil par région", "FCFA/kg")
ggsave(file.path(paths$maps, "carte_prix_conso_mil.png"), c2, width = 8, height = 7, dpi = 150, bg = "white")

c3 <- creer_carte(prix_marche_region, "prix_moyen", "Prix de marché du mil par région", "FCFA/kg")
ggsave(file.path(paths$maps, "carte_prix_marche_mil.png"), c3, width = 8, height = 7, dpi = 150, bg = "white")

c4 <- creer_carte(prix_region, "marge_conso_region", "Marge commerciale du mil par région", "FCFA/kg")
ggsave(file.path(paths$maps, "carte_marge_mil.png"), c4, width = 8, height = 7, dpi = 150, bg = "white")

cat("Cartes sauvegardées dans outputs/maps/\n")
cat("\nÉtape 2 terminée.\n")

# Sauvegarde des prix régionaux pour le dashboard
saveRDS(prix_prod_region, here::here("data", "processed", "prix_prod_region.rds"))
saveRDS(prix_conso_region, here::here("data", "processed", "prix_conso_region.rds"))
saveRDS(prix_marche_region, here::here("data", "processed", "prix_marche_region.rds"))