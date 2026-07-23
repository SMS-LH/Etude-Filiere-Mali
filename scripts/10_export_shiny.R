# ==============================================================================
# 10_export_shiny.R – Version définitive (données réelles, rendements via table externe)
# ==============================================================================

source(here::here("scripts", "00_setup.R"))

cat("Préparation des données pour le dashboard Shiny...\n")
create_dir(here::here("shiny", "data"))

# ------------------------------------------------------------------------------
# 1. Base typologique et climat
# ------------------------------------------------------------------------------
base_typo <- readRDS(here::here("data", "processed", "typologie_mil.rds"))
climat    <- readRDS(here::here("data", "processed", "base_mil_climat.rds"))

base_clim <- base_typo %>%
  select(hhid, region) %>%
  left_join(climat, by = "hhid")

corresp_region <- data.frame(
  region = 1:9,
  NAME_1 = c("Kayes","Koulikoro","Sikasso","Ségou","Mopti",
             "Timbuktu","Gao","Kidal","Bamako"),
  stringsAsFactors = FALSE
)

pluie_region <- base_clim %>%
  group_by(region) %>%
  summarise(pluie_moyenne = mean(pluie_2021, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    region_nom = corresp_region$NAME_1[match(region, corresp_region$region)],
    cereale_dominante = case_when(
      region %in% c(1,2) ~ "Sorgho",
      region == 3 ~ "Maïs",
      region %in% c(4,5) ~ "Mil",
      TRUE ~ "Riz"
    )
  )

taux_autoconso_mil <- base_typo %>%
  filter(groupe == "Producteur-consommateur") %>%
  with(weighted.mean(taux_autoconso, hhweight, na.rm = TRUE)) * 100

# ------------------------------------------------------------------------------
# 2. Comparaison céréales (étape 0)
# ------------------------------------------------------------------------------
fichier_synthese <- here::here("outputs", "tables", "synthese_produits.xlsx")
if (file.exists(fichier_synthese)) {
  synthese <- readxl::read_xlsx(fichier_synthese, sheet = "Comparaison produits", skip = 2)
  if (colnames(synthese)[1] == "...1") colnames(synthese)[1] <- "Produit"
  
  comparaison_cereales <- synthese %>%
    rename(
      cereale              = Produit,
      prevalence           = Prev_conso,
      part_budget          = Part_budget,
      part_cal             = Part_calories_cereales,
      producteurs          = Prev_prod,
      taux_commerc         = Tx_commercialisation,
      superficie_ha        = Superficie_ha,
      production_tonnes    = Production_tonnes,
      rendement_kg_ha      = Rendement_kg_ha,
      valeur_ventes        = Valeur_ventes,
      prix_fcfa_kg         = Prix_FCFA_kg,
      prod_fao_tonnes      = Prod_FAO_tonnes,
      balance_com_tonnes   = Balance_com_tonnes,
      dispo_kg_capita      = Dispo_kg_capita_an,
      kcal_capita_jour     = Kcal_capita_jour
    ) %>%
    mutate(autoconso = if_else(cereale == "Mil", taux_autoconso_mil, NA_real_))
  
  # CORRECTIF : lecture du fichier FAOSTAT trade propre (.xls, plus de parsing
  # manuel de CSV corrompu), et item "Riz, paddy (riz blanchi équivalent)"
  # pour le riz -- exactement le même correctif que celui déjà appliqué dans
  # 02_etape0_choix_produit.R. L'ancien code ici pointait encore vers l'item
  # "Riz" (paddy brut, CPC 0113), qui sous-estime massivement les
  # importations réelles de riz (usiné/brisures/décortiqué non comptés).
  trade_file <- here::here("data", "raw", "faostat", "FAOSTAT_mali_trade.xls")
  if (file.exists(trade_file)) {
    faostat_trade <- readxl::read_excel(trade_file)
    
    fao_produit_trade <- c(
      "Mil"    = "Mils",
      "Sorgho" = "Sorgho",
      "Riz"    = "Riz, paddy (riz blanchi équivalent)",
      "Maïs"   = "Maïs"
    )
    
    import_df <- data.frame(
      produit = names(fao_produit_trade),
      ImportQuantity = sapply(names(fao_produit_trade), function(nom) {
        ligne <- faostat_trade %>%
          filter(Produit == fao_produit_trade[[nom]], Année == 2021,
                 Élément == "Importations - quantité")
        if (nrow(ligne) > 0) sum(ligne$Valeur, na.rm = TRUE) else 0
      }),
      stringsAsFactors = FALSE
    )
    
    prod_df <- synthese %>%
      select(produit = Produit, Production = Prod_FAO_tonnes) %>%
      mutate(Production = as.numeric(Production), produit = as.character(produit))
    
    fao_commerce <- prod_df %>%
      left_join(import_df, by = "produit") %>%
      mutate(
        ImportQuantity = if_else(is.na(ImportQuantity), 0, ImportQuantity),
        Production     = round(Production / 1000, 1),
        ImportQuantity = round(ImportQuantity / 1000, 2)
      ) %>%
      rename(`Import Quantity` = ImportQuantity)
    
  } else {
    warning("Fichier ", trade_file, " introuvable -- importations mises à 0.")
    fao_commerce <- synthese %>%
      select(produit = Produit, Production = Prod_FAO_tonnes) %>%
      mutate(`Import Quantity` = 0, produit = as.character(produit))
  }
  
  # Disponibilité
  fao_dispo <- synthese %>%
    select(produit = Produit, kg_capita_an = Dispo_kg_capita_an) %>%
    mutate(kg_capita_an = as.numeric(kg_capita_an))
  
} else {
  comparaison_cereales <- NULL
  fao_commerce <- NULL
  fao_dispo    <- NULL
}

# Part des importations dans la production, pour chaque céréale (utilisé pour
# les textes dynamiques du dashboard, notamment "le riz dépend des
# importations à X%")
part_import_cereales <- if (!is.null(fao_commerce)) {
  fao_commerce %>%
    mutate(part_import_pct = round(100 * `Import Quantity` / Production, 1)) %>%
    select(produit, part_import_pct)
} else {
  NULL
}

# ------------------------------------------------------------------------------
# 3. Ménages et caractéristiques par groupe
# ------------------------------------------------------------------------------
if (!"quintile" %in% names(base_typo)) {
  base_typo <- base_typo %>%
    mutate(quintile = cut(pcexp,
                          breaks = quantile(pcexp, probs = seq(0, 1, 0.2), na.rm = TRUE),
                          labels = 1:5, include.lowest = TRUE))
}
menages <- base_typo %>%
  transmute(hhid, hhweight, milieu, region, quintile, groupe,
            fies_score, hdds_score, pcexp)

base_typo <- base_typo %>%
  mutate(
    alphabet = if_else(educ_chef > 1 & educ_chef != 9999, 1, 0),
    rural    = if_else(milieu == 2, 1, 0)
  )
caract_groupes <- base_typo %>%
  group_by(groupe) %>%
  summarise(
    niveau_vie = weighted.mean(pcexp, hhweight, na.rm = TRUE),
    age_chef   = weighted.mean(age_chef, hhweight, na.rm = TRUE),
    taille     = weighted.mean(hhsize, hhweight, na.rm = TRUE),
    alphabet   = weighted.mean(alphabet, hhweight, na.rm = TRUE) * 100,
    rural      = weighted.mean(rural, hhweight, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# 4. Rendement (table de conversion externe EAC, méthode étape 1)
# ------------------------------------------------------------------------------
# Charger les données converties de l'étape 1
s16c_conv <- readRDS(here::here("data", "processed", "s16c_converti.rds"))
s16d_conv <- readRDS(here::here("data", "processed", "s16d_converti.rds"))
s16a_raw  <- load_dta(paths$raw_menage, "s16a_me_mli2021.dta") %>%
  mutate(hhid = paste(grappe, menage, vague, sep = "_"))

# Production de mil (code 1) par ménage
prod_mil <- s16d_conv %>%
  filter(s16dq01 == 1) %>%
  mutate(prod_totale_kg = coalesce(conso_kg,0) + coalesce(don_kg,0) +
           coalesce(vente_kg,0) + coalesce(stock_kg,0)) %>%
  group_by(hhid) %>%
  summarise(production_kg = sum(prod_totale_kg, na.rm = TRUE), .groups = "drop")

# Superficie des parcelles de mil (méthode proportionnelle)
s16c_raw <- load_dta(paths$raw_menage, "s16c_me_mli2021.dta") %>%
  mutate(hhid = paste(grappe, menage, vague, sep = "_"))

parcelles_mil <- s16c_raw %>%
  select(hhid, s16aq02 = s16cq02, s16aq03 = s16cq03, s16cq04, s16cq08) %>%
  filter(s16cq04 == 1) %>%
  left_join(s16a_raw %>% select(hhid, s16aq02, s16aq03,
                                sup_decl = s16aq09a, sup_decl_unite = s16aq09b,
                                sup_gps = s16aq47),
            by = c("hhid", "s16aq02", "s16aq03")) %>%
  mutate(
    sup_ha = case_when(
      !is.na(sup_gps) & sup_gps > 0 ~ sup_gps,
      sup_decl_unite == 1 ~ sup_decl,
      sup_decl_unite == 2 ~ sup_decl / 10000,
      TRUE ~ NA_real_
    ),
    pct = if_else(is.na(s16cq08) | s16cq08 == 0, 100, s16cq08),
    sup_culture_ha = sup_ha * pct / 100
  )

sup_mil <- parcelles_mil %>%
  group_by(hhid) %>%
  summarise(superficie_ha = sum(sup_culture_ha, na.rm = TRUE), .groups = "drop")

# Rendement winsorisé -- on garde hhweight cette fois pour permettre un
# rendement moyen PONDÉRÉ cohérent avec le libellé du dashboard ("pondéré")
rend_menage <- inner_join(prod_mil, sup_mil, by = "hhid") %>%
  left_join(base_typo %>% select(hhid, hhweight), by = "hhid") %>%
  filter(superficie_ha > 0, production_kg > 0) %>%
  mutate(
    rdt_brut = production_kg / superficie_ha,
    rendement_wins = winsorize(rdt_brut, probs = c(0.01, 0.99))
  ) %>%
  select(hhid, hhweight, superficie_ha, rendement_wins)

# Rendement moyen par région (priorité à l'Excel de l'étape 1)
fichier_rdt_region <- here::here("outputs", "tables", "resultats_etape1.xlsx")
if (file.exists(fichier_rdt_region)) {
  rdt_region_excel <- readxl::read_xlsx(fichier_rdt_region, sheet = "Rendement par région", skip = 2)
  if (all(c("Région", "Rendement_moyen") %in% names(rdt_region_excel))) {
    rendement_par_region <- rdt_region_excel %>%
      transmute(region = Région, rendement = Rendement_moyen)
  } else {
    rendement_par_region <- rend_menage %>%
      left_join(base_typo %>% select(hhid, region), by = "hhid") %>%
      group_by(region) %>%
      summarise(rendement = weighted.mean(rendement_wins, hhweight, na.rm = TRUE), .groups = "drop")
  }
} else {
  rendement_par_region <- rend_menage %>%
    left_join(base_typo %>% select(hhid, region), by = "hhid") %>%
    group_by(region) %>%
    summarise(rendement = weighted.mean(rendement_wins, hhweight, na.rm = TRUE), .groups = "drop")
}

# Coefficients du modèle de rendement
fichier_modele_ols <- here::here("data", "processed", "modele_rendement_ols.rds")
fichier_tab_rdt    <- here::here("data", "processed", "modeles_rendement_mil.rds")

if (file.exists(fichier_modele_ols)) {
  modele_ols <- readRDS(fichier_modele_ols)
  coefs_rendement <- broom::tidy(modele_ols) %>%
    filter(term != "(Intercept)") %>%
    select(variable = term, coef = estimate)
} else if (file.exists(fichier_tab_rdt)) {
  tab_rdt <- readRDS(fichier_tab_rdt)
  if (is.data.frame(tab_rdt) && "Variable" %in% names(tab_rdt) && "OLS" %in% names(tab_rdt)) {
    tab_rdt <- tab_rdt %>% filter(Variable != "(Intercept)")
    coefs_rendement <- tab_rdt %>%
      transmute(
        variable = Variable,
        coef     = as.numeric(gsub("[*]+", "", trimws(OLS)))
      )
  } else {
    coefs_rendement <- NULL
  }
} else {
  coefs_rendement <- NULL
}

# ------------------------------------------------------------------------------
# 5. Marges et prix régionaux
# ------------------------------------------------------------------------------
prix_prod_path  <- here::here("data", "processed", "prix_prod_region.rds")
prix_conso_path <- here::here("data", "processed", "prix_conso_region.rds")
prix_marche_path<- here::here("data", "processed", "prix_marche_region.rds")
if (file.exists(prix_prod_path) && file.exists(prix_conso_path) && file.exists(prix_marche_path)) {
  prix_prod_reg  <- readRDS(prix_prod_path)
  prix_conso_reg <- readRDS(prix_conso_path)
  prix_marche_reg <- readRDS(prix_marche_path)
  marges_off <- prix_prod_reg %>%
    select(region, prix_prod = prix_moyen) %>%
    left_join(prix_conso_reg %>% select(region, prix_conso = prix_moyen), by = "region") %>%
    left_join(prix_marche_reg %>% select(region, prix_marche = prix_moyen), by = "region") %>%
    mutate(marge_marche = prix_marche - prix_prod)
  
  # KPI nationaux -- moyenne simple des régions, cohérente avec les tableaux
  # régionaux affichés sur la carte. Si tu as gardé les médianes nationales
  # calculées dans 04_etape2_commercialisation.R (95 obs, médiane 200/375),
  # remplace ces deux lignes par une lecture directe de ces valeurs pour
  # rester rigoureusement cohérent avec l'EDA de l'étape 2.
  prix_prod_national   <- mean(marges_off$prix_prod, na.rm = TRUE)
  prix_marche_national <- mean(marges_off$prix_marche, na.rm = TRUE)
} else {
  marges_off <- NULL
  prix_prod_national   <- NA_real_
  prix_marche_national <- NA_real_
}

# ------------------------------------------------------------------------------
# 6. Canaux de vente et stockage
# ------------------------------------------------------------------------------
s16d_raw <- load_dta(paths$raw_menage, "s16d_me_mli2021.dta") %>%
  mutate(hhid = paste(grappe, menage, vague, sep = "_"))

canaux <- s16d_raw %>%
  filter(s16dq01 == 1, s16dq04 == 1) %>%
  count(canal = as.character(s16dq08)) %>%
  mutate(
    part = round(100 * n / sum(n), 1),
    canal = recode(canal,
                   "1" = "Marché", "2" = "Ménage/Particulier",
                   "4" = "Opérateur privé", .default = "Autre")
  ) %>%
  select(canal, part)

stockage <- s16d_raw %>%
  filter(s16dq01 == 1, s16dq12 == 1) %>%
  count(methode = as.character(s16dq11)) %>%
  mutate(
    part = round(100 * n / sum(n), 1),
    methode = recode(methode,
                     "1" = "Grenier dans concession", "2" = "Grenier hors concession",
                     "3" = "Magasin", "4" = "Hangar", "5" = "Toit de la maison",
                     "6" = "Aucune méthode", .default = "Autre")
  ) %>%
  filter(!is.na(methode)) %>%
  select(methode, part)

# ------------------------------------------------------------------------------
# 7. Sécurité alimentaire et impact
# ------------------------------------------------------------------------------
secu_par_groupe <- base_typo %>%
  group_by(groupe) %>%
  summarise(
    prev_moderee = weighted.mean(fies_modere, hhweight, na.rm = TRUE) * 100,
    prev_severe  = weighted.mean(fies_severe, hhweight, na.rm = TRUE) * 100,
    hdds_moyen   = weighted.mean(hdds_score, hhweight, na.rm = TRUE),
    .groups = "drop"
  )

# KPI nationaux de sécurité alimentaire (tous ménages, pas juste par groupe) --
# calculés directement sur base_typo (donc sur le fies_score corrigé) plutôt
# que codés en dur dans l'UI.
insecurite_moderee_nat <- weighted.mean(base_typo$fies_modere, base_typo$hhweight, na.rm = TRUE) * 100
insecurite_severe_nat  <- weighted.mean(base_typo$fies_severe, base_typo$hhweight, na.rm = TRUE) * 100
hdds_national           <- weighted.mean(base_typo$hdds_score, base_typo$hhweight, na.rm = TRUE)

fichier_impact <- here::here("outputs", "tables", "impact_filiere_mil.xlsx")
if (file.exists(fichier_impact)) {
  impact_mod <- readxl::read_xlsx(fichier_impact, sheet = "Modèles", skip = 2)
  if (colnames(impact_mod)[1] == "...1") colnames(impact_mod)[1] <- "term"
  coefs <- data.frame(
    modele   = rep(c("FIES", "HDDS"), each = nrow(impact_mod) - 1),
    variable = rep(impact_mod$term[-1], 2),
    coef     = c(as.numeric(impact_mod[["FIES (base)"]][-1]),
                 as.numeric(impact_mod[["HDDS (base)"]][-1]))
  ) %>% filter(variable != "(Intercept)")
} else {
  library(fixest)
  mod_fies <- feols(fies_score ~ producteur_mil + age_chef + educ_chef + hhsize + pcexp + milieu + region,
                    data = base_typo, weights = ~ hhweight, cluster = ~ grappe)
  mod_hdds <- feols(hdds_score ~ producteur_mil + age_chef + educ_chef + hhsize + pcexp + milieu + region,
                    data = base_typo, weights = ~ hhweight, cluster = ~ grappe)
  coefs <- bind_rows(
    broom::tidy(mod_fies) %>% mutate(modele = "FIES"),
    broom::tidy(mod_hdds) %>% mutate(modele = "HDDS")
  ) %>%
    filter(term != "(Intercept)") %>%
    transmute(modele, variable = term, coef = estimate)
}

# ------------------------------------------------------------------------------
# 8. KPI nationaux consolidés -- NOUVEAU
# ------------------------------------------------------------------------------
# Tous les chiffres qui étaient codés en dur dans app.R sont regroupés ici et
# calculés dynamiquement, pour que l'UI n'ait plus jamais à être mise à jour
# à la main après un recalcul du pipeline.
kpis <- list(
  autoconso_mil_pct       = round(taux_autoconso_mil, 1),
  riz_import_pct          = if (!is.null(part_import_cereales)) {
    round(part_import_cereales$part_import_pct[part_import_cereales$produit == "Riz"], 1)
  } else NA_real_,
  rendement_moyen_pondere = round(weighted.mean(rend_menage$rendement_wins, rend_menage$hhweight, na.rm = TRUE)),
  rendement_median        = round(median(rend_menage$rendement_wins, na.rm = TRUE)),
  superficie_moyenne_ha   = round(mean(sup_mil$superficie_ha, na.rm = TRUE), 1),
  prix_producteur         = round(prix_prod_national),
  prix_marche             = round(prix_marche_national),
  marge_fcfa_kg           = round(prix_marche_national - prix_prod_national),
  part_producteur_pct     = round(100 * prix_prod_national / prix_marche_national, 1),
  insecurite_totale_pct   = round(insecurite_moderee_nat, 1),
  insecurite_severe_pct   = round(insecurite_severe_nat, 1),
  hdds_national           = round(hdds_national, 1)
)

cat("\n--- KPI nationaux calculés pour le dashboard ---\n")
print(kpis)

# ------------------------------------------------------------------------------
# 9. Assemblage final
# ------------------------------------------------------------------------------
dash <- list(
  comparaison_cereales = comparaison_cereales,
  fao_commerce         = fao_commerce,
  fao_dispo            = fao_dispo,
  pluie_region         = pluie_region,
  menages              = menages,
  caract_groupes       = caract_groupes,
  rendement_menage     = rend_menage,
  rendement_par_region = rendement_par_region,
  coefs_rendement      = coefs_rendement,
  marges_off           = marges_off,
  canaux               = canaux,
  stockage             = stockage,
  secu_par_groupe      = secu_par_groupe,
  coefs                = coefs,
  kpis                 = kpis
)

saveRDS(dash, here::here("shiny", "data", "dashboard_data.rds"))

# ------------------------------------------------------------------------------
# 10. Métadonnées
# ------------------------------------------------------------------------------
metadata_indicators <- data.frame(
  Indicateur = c(
    "Prev_conso", "Part_budget", "Contrib_cal", "Prev_prod",
    "Superficie_ha", "Production_tonnes", "Rendement_kg_ha",
    "Valeur_ventes", "Tx_commercialisation", "Prix_FCFA_kg",
    "Prod_FAO_tonnes", "Balance_com_tonnes",
    "fies_moyen", "hdds_moyen", "fies_modere_pct", "fies_severe_pct",
    "hhsize_moy", "pcexp_med", "pauvre_pct",
    "age_chef_moy", "sexe_chef_homme_pct", "educ_chef_pct",
    "superf_moy", "bovins_moy", "ovins_moy", "volailles_moy", "equip_moy",
    "intrants_moy", "credit_pct", "coop_pct", "chocs_moy",
    "taux_autoconso_moy", "taux_com_moy", "part_budget_moy",
    "pluie_2021", "pluie_saison_2021", "temp_moy"
  ),
  Description = c(
    "Proportion de ménages consommateurs (%)",
    "Part du produit dans la dépense alimentaire totale (%)",
    "Contribution calorique du produit (%)",
    "Proportion de ménages producteurs (%)",
    "Superficie totale cultivée (ha)",
    "Production totale estimée (tonnes)",
    "Rendement moyen winsorisé (kg/ha)",
    "Valeur totale des ventes (FCFA)",
    "Taux de commercialisation (%)",
    "Prix implicite au producteur (FCFA/kg)",
    "Production nationale FAO (tonnes)",
    "Balance commerciale FAO (tonnes)",
    "Score FIES moyen (0-8)",
    "Score HDDS moyen (0-12)",
    "Insécurité alimentaire modérée (%)",
    "Insécurité alimentaire sévère (%)",
    "Taille moyenne du ménage",
    "Dépense médiane par tête (FCFA)",
    "Proportion de ménages pauvres (%)",
    "Âge moyen du chef de ménage",
    "Proportion de chefs masculins (%)",
    "Proportion de chefs avec éducation (%)",
    "Superficie agricole moyenne (ha)",
    "Nombre moyen de bovins",
    "Nombre moyen d'ovins/caprins",
    "Nombre moyen de volailles",
    "Nombre moyen d'équipements agricoles",
    "Valeur moyenne des intrants (FCFA)",
    "Accès au crédit (%)",
    "Accès à une coopérative (%)",
    "Nombre moyen de chocs subis",
    "Taux d'autoconsommation (%)",
    "Taux de commercialisation (%)",
    "Part budgétaire du mil (%)",
    "Pluviométrie annuelle 2021 (mm)",
    "Pluviométrie saisonnière 2021 (mm)",
    "Température moyenne (°C)"
  ),
  stringsAsFactors = FALSE
)

write.csv(metadata_indicators, here::here("shiny", "data", "metadata_indicators.csv"), row.names = FALSE)

cat("Dashboard data sauvegardée avec données réelles (rendements via table externe).\n")