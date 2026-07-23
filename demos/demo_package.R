# ==============================================================================
# demo_package.R
# Script de démonstration du package filiere.mali sur données réelles
# Utilise devtools::load_all() – ne nécessite pas d'installation préalable.
# ==============================================================================

devtools::load_all("package/filiere.mali")
library(here)

# ------------------------------------------------------------------------------
# 1. Chargement de fichiers via load_filiere
# ------------------------------------------------------------------------------
cat("\n--- 1. load_filiere ---\n")

s00  <- load_filiere(here::here("data", "raw", "EHCVM", "menage", "s00_me_mli2021.dta"))
s07b <- load_filiere(here::here("data", "raw", "EHCVM", "menage", "s07b_me_mli2021.dta"))
s08a <- load_filiere(here::here("data", "raw", "EHCVM", "menage", "s08a_me_mli2021.dta"))
pond <- load_filiere(here::here("data", "raw", "EHCVM", "auxiliaires", "ehcvm_ponderations_mli2021.dta"))
s08a <- load_filiere(here::here("data", "raw", "EHCVM", "menage", "s08a_me_mli2021.dta"),
                     ponderations = pond)

cat("s00 :", nrow(s00), "ménages\n")
cat("s07b :", nrow(s07b), "lignes\n")
cat("s08a avec poids :", nrow(s08a), "ménages\n")

# ------------------------------------------------------------------------------
# 2. calc_fies
# ------------------------------------------------------------------------------
cat("\n--- 2. calc_fies ---\n")
questions_fies <- paste0("s08aq0", 1:8)
s08a <- calc_fies(s08a, questions = questions_fies)
cat("Score FIES moyen :", round(mean(s08a$score_fies), 2), "\n")
cat("Part insécurité modérée :", round(mean(s08a$insecurite_moderee) * 100, 1), "%\n")
cat("Part insécurité sévère  :", round(mean(s08a$insecurite_severe) * 100, 1), "%\n")

# ------------------------------------------------------------------------------
# 3. calc_hdds
# ------------------------------------------------------------------------------
cat("\n--- 3. calc_hdds ---\n")
map_hdds <- data.frame(
  produit = c(1:26, 169, 167, 168, 123:133, 88:108, 71:87, 176, 27:39, 170, 171,
              60, 40:51, 172, 173, 179, 109:122, 153, 52:59, 174,
              61:70, 175, 134:138, 139:154),
  groupe = c(rep(1, 29), rep(2, 11), rep(3, 21), rep(4, 18), rep(5, 16),
             6, rep(7, 15), rep(8, 15), rep(9, 9), rep(10, 11),
             rep(11, 5), rep(12, 15))
)
hdds <- calc_hdds(s07b, col_menage = c("grappe", "menage"), col_produit = "s07bq01",
                  correspondance = map_hdds, col_consomme = "s07bq02")
cat("HDDS moyen :", round(mean(hdds$hdds), 2), "\n")

# ------------------------------------------------------------------------------
# 4. calc_rendement
# ------------------------------------------------------------------------------

cat("\n--- 4. calc_rendement ---\n")
s16a <- load_filiere(here::here("data", "raw", "EHCVM", "menage", "s16a_me_mli2021.dta"))
s16d <- load_filiere(here::here("data", "raw", "EHCVM", "menage", "s16d_me_mli2021.dta"))

s16a_mil <- dplyr::filter(s16a, s16aq08 == 1)
sup_menage <- s16a_mil %>%
  dplyr::group_by(grappe, menage) %>%
  dplyr::summarise(
    superficie_ha = sum(
      dplyr::if_else(!is.na(s16aq47) & s16aq47 > 0, s16aq47,
                     dplyr::if_else(s16aq09b == 1, s16aq09a,
                                    dplyr::if_else(s16aq09b == 2, s16aq09a / 10000, NA_real_))),
      na.rm = TRUE
    ),
    .groups = "drop"
  )

data("conversion", package = "filiere.mali")

# Production totale = somme des 4 usages déclarés en Section 16D
# (autoconsommation, dons, ventes, stock), chacun converti en kg via la table
# de conversion du package.
s16d_mil <- s16d %>%
  dplyr::filter(s16dq01 == 1) %>%
  tidyr::pivot_longer(
    cols = c(s16dq02a, s16dq03a, s16dq05a, s16dq13a),
    names_to = "type_qte", values_to = "uml"
  ) %>%
  dplyr::mutate(unite = dplyr::case_when(
    type_qte == "s16dq02a" ~ s16dq02b,
    type_qte == "s16dq03a" ~ s16dq03b,
    type_qte == "s16dq05a" ~ s16dq05b,
    type_qte == "s16dq13a" ~ s16dq13b
  )) %>%
  dplyr::filter(!is.na(uml), uml > 0) %>%
  dplyr::left_join(conversion, by = c("s16dq01" = "codpr", "unite" = "unite")) %>%
  dplyr::mutate(production_kg = uml * ratio_kg_uml)

prod_menage <- s16d_mil %>%
  dplyr::group_by(grappe, menage) %>%
  dplyr::summarise(production_kg = sum(production_kg, na.rm = TRUE), .groups = "drop")

rend_data <- dplyr::inner_join(sup_menage, prod_menage, by = c("grappe", "menage"))
rend_data <- calc_rendement(rend_data, culture = "millet",
                            col_production = "production_kg",
                            col_superficie = "superficie_ha")
cat("Rendement brut médian :", round(median(rend_data$rendement_brut, na.rm = TRUE)), "kg/ha\n")
cat("Rendement plafonné médian :", round(median(rend_data$rendement_plafonne, na.rm = TRUE)), "kg/ha\n")

# ------------------------------------------------------------------------------
# 5. profil_menage
# ------------------------------------------------------------------------------
cat("\n--- 5. profil_menage ---\n")
producteurs <- s16a_mil %>% dplyr::distinct(grappe, menage) %>% dplyr::mutate(producteur_mil = 1)
consommateurs <- s07b %>%
  dplyr::filter(s07bq01 %in% c(7,14,15), s07bq02 == 1) %>%
  dplyr::distinct(grappe, menage) %>%
  dplyr::mutate(consommateur_mil = 1)
typologie <- s00 %>%
  dplyr::select(grappe, menage) %>%
  dplyr::left_join(producteurs, by = c("grappe", "menage")) %>%
  dplyr::left_join(consommateurs, by = c("grappe", "menage")) %>%
  dplyr::mutate(
    dplyr::across(c(producteur_mil, consommateur_mil), ~ dplyr::if_else(is.na(.x), 0, .x)),
    groupe = dplyr::case_when(
      producteur_mil == 1 & consommateur_mil == 1 ~ "Prod-Cons",
      producteur_mil == 1 & consommateur_mil == 0 ~ "Prod-seul",
      producteur_mil == 0 & consommateur_mil == 1 ~ "Cons-seul",
      TRUE ~ "Ni prod ni cons"
    )
  )

profil <- profil_menage(typologie, groupe = "groupe",
                        vars_moyennes = c("producteur_mil", "consommateur_mil"),
                        vars_parts = c("producteur_mil"))
print(profil)

# ------------------------------------------------------------------------------
# 6. prix_chaine
# ------------------------------------------------------------------------------
cat("\n--- 6. prix_chaine ---\n")
ventes_mil <- s16d %>%
  dplyr::filter(s16dq01 == 1, s16dq04 == 1 | s16dq06 > 0) %>%
  dplyr::left_join(conversion, by = c("s16dq01" = "codpr", "s16dq05b" = "unite")) %>%
  dplyr::mutate(qte_kg = s16dq05a * ratio_kg_uml)
prix <- prix_chaine(ventes_mil, col_montant = "s16dq06", col_quantite = "qte_kg",
                    methode_nettoyage = "iqr")
cat("Prix producteur médian :", round(median(prix$prix_unitaire, na.rm = TRUE)), "FCFA/kg\n")

# ------------------------------------------------------------------------------
# 7. carte_filiere
# ------------------------------------------------------------------------------
cat("\n--- 7. carte_filiere ---\n")
mali_regions <- sf::st_read(here::here("data", "raw", "spatial", "gadm", "gadm41_MLI_1.shp"), quiet = TRUE)
s00_regions <- s00 %>% dplyr::select(grappe, menage, s00q01)
rend_region <- rend_data %>%
  dplyr::left_join(s00_regions, by = c("grappe", "menage")) %>%
  dplyr::group_by(region = s00q01) %>%
  dplyr::summarise(rdt_moyen = mean(rendement_plafonne, na.rm = TRUE)) %>%
  dplyr::mutate(NAME_1 = c("1"="Kayes","2"="Koulikoro","3"="Sikasso","4"="Ségou",
                           "5"="Mopti","6"="Tombouctou","7"="Gao","8"="Kidal","9"="Bamako")[as.character(region)])
carte <- carte_filiere(rend_region, fond = mali_regions, col_valeur = "rdt_moyen",
                       col_region_data = "NAME_1", titre = "Rendement mil (kg/ha)")
print(carte)

# ------------------------------------------------------------------------------
# 8. reg_filiere
# ------------------------------------------------------------------------------
cat("\n--- 8. reg_filiere ---\n")
impact_data <- s08a %>%
  dplyr::left_join(rend_data, by = c("grappe", "menage")) %>%
  dplyr::left_join(s00 %>% dplyr::select(grappe, menage, milieu = s00q04), by = c("grappe", "menage"))
mod <- reg_filiere(impact_data, outcome = "score_fies",
                   filiere_vars = "rendement_plafonne",
                   controls = "milieu")
print(mod)

cat("\n=== Démonstration terminée avec succès ===\n")