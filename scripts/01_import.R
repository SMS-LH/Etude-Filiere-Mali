# ==============================================================================
# 01_import.R
# Import et conversion des données EHCVM Mali 2021/2022
# ==============================================================================

source(here::here("scripts", "00_setup.R"))

# ------------------------------------------------------------------------------
# 1. Identifiant ménage unique
# ------------------------------------------------------------------------------

creer_hhid <- function(df) {
  df %>% mutate(hhid = paste(grappe, menage, vague, sep = "_"))
}

# ------------------------------------------------------------------------------
# 2. Table ménage socle
# ------------------------------------------------------------------------------

cat("Import de la table ménage socle...\n")

s00 <- load_dta(paths$raw_menage, "s00_me_mli2021.dta") %>%
  creer_hhid() %>%
  select(hhid, grappe, menage, vague,
         region = s00q01, milieu = s00q04,
         type_menage = s00q07a, panel = PanelHH,
         latitude = GPS__Latitude, longitude = GPS__Longitude)

s01 <- load_dta(paths$raw_menage, "s01_me_mli2021.dta") %>%
  creer_hhid() %>%
  filter(s01q02 == 1) %>%
  select(hhid,
         sexe_chef = s01q01, age_chef = s01q04a,
         religion_chef = s01q14, ethnie_chef = s01q16,
         educ_chef = s01q25, matri_chef = s01q07)

pond <- load_dta(paths$raw_auxiliaires, "ehcvm_ponderations_mli2021.dta") %>%
  select(grappe, menage, hhweight)

menage_socle <- s00 %>%
  left_join(s01, by = "hhid") %>%
  left_join(pond, by = c("grappe", "menage"))

save_rds(menage_socle, paths$processed, "menage_socle.rds")
cat("  ménage_socle :", nrow(menage_socle), "lignes\n")

# ------------------------------------------------------------------------------
# 3. Table consommation alimentaire
# ------------------------------------------------------------------------------

cat("Import de la table consommation alimentaire...\n")

s07b <- load_dta(paths$raw_menage, "s07b_me_mli2021.dta") %>%
  creer_hhid() %>%
  select(hhid, codpr = s07bq01, conso_7j = s07bq02,
         qte_conso = s07bq03a, unite_conso = s07bq03b, taille_conso = s07bq03c,
         qte_autoconso = s07bq04, qte_cadeau = s07bq05,
         dernier_achat = s07bq06,
         qte_achetee = s07bq07a, unite_achetee = s07bq07b, taille_achetee = s07bq07c,
         valeur_achat = s07bq08)

cal <- load_dta(paths$raw_auxiliaires, "calorie_conversion_wa_2021.dta") %>%
  select(codpr, prodlab, refuse, cal)

conso_alimentaire <- suppressWarnings(
  s07b %>% left_join(cal, by = "codpr")
)

save_rds(conso_alimentaire, paths$processed, "conso_alimentaire.rds")
cat("  conso_alimentaire :", nrow(conso_alimentaire), "lignes\n")

# ------------------------------------------------------------------------------
# 4. Production agricole avec conversion externe
# ------------------------------------------------------------------------------

cat("Import et conversion des données de production...\n")

# 4.1 Import des sections brutes (on garde grappe, menage)
s16a <- load_dta(paths$raw_menage, "s16a_me_mli2021.dta") %>%
  creer_hhid() %>%
  select(hhid, grappe, menage, s16aq02, s16aq03, s16aq08,
         sup_decl = s16aq09a, sup_decl_unite = s16aq09b,
         sup_gps = s16aq47,
         source_eau = s16aq17, fertilite = s16aq20, mode_labour = s16aq44)

s16c <- load_dta(paths$raw_menage, "s16c_me_mli2021.dta") %>%
  creer_hhid() %>%
  select(hhid, grappe, menage, s16cq02, s16cq03, s16cq04, s16cq08,
         qte_recoltee_uml = s16cq16a, uml_recolte = s16cq16b,
         qte_recoltee_kg = s16cq16c, etat_recolte = s16cq16d)

s16d <- load_dta(paths$raw_menage, "s16d_me_mli2021.dta") %>%
  creer_hhid() %>%
  select(hhid, grappe, menage, s16dq01,
         qte_conso_uml = s16dq02a, uml_conso = s16dq02b,
         qte_don_uml  = s16dq03a, uml_don  = s16dq03b,
         vente_oui    = s16dq04,
         qte_vendue_uml = s16dq05a, uml_vente = s16dq05b,
         kg_vente_est = s16dq05c,
         revenu_vente = s16dq06,
         stock_oui    = s16dq12,
         qte_stock_uml = s16dq13a, uml_stock = s16dq13b,
         diff_ecoul   = s16dq19)

# 4.2 Géolocalisation des ménages (grappe, menage -> region, milieu)
geo_menage <- s00 %>%
  transmute(grappe = as.integer(grappe),
            menage = as.integer(menage),
            region = as.integer(region),
            milieu = as.integer(milieu))

# 4.3 Table de conversion externe
chemin_eac <- file.path(paths$raw_reference, "table_conversion_4unites.xlsx")
if (!file.exists(chemin_eac)) stop("Fichier introuvable : ", chemin_eac)

eac <- readxl::read_xlsx(chemin_eac)
eac <- eac %>% filter(!is.na(produitID_ehcvm))

bloc_unite_eac <- function(nom_unite_eac, code_s16) {
  eac %>%
    filter(uniteNom == nom_unite_eac) %>%
    transmute(
      produit_code = as.integer(produitID_ehcvm),
      unite_code   = code_s16,
      region       = as.integer(region),
      milieu       = as.integer(milieu),
      facteur_kg   = as.numeric(poids)
    )
}

bloc_eac <- bind_rows(
  bloc_unite_eac("Charretée", 3),
  bloc_unite_eac("Charretée", 4),
  bloc_unite_eac("Bassine",   11),
  bloc_unite_eac("Panier",    12),
  bloc_unite_eac("Gerbe",     10)
)

bloc_standard <- tibble(
  unite_code = c(1L, 5L, 6L, 7L),
  facteur_kg = c(1, 25, 50, 100)
)

table_conv_nat <- bloc_eac %>%
  group_by(produit_code, unite_code) %>%
  summarise(facteur_kg_nat = mean(facteur_kg, na.rm = TRUE), .groups = "drop")

# 4.4 Fonction de conversion
convertir_en_kg <- function(quantite, unite_code, produit_code, region, milieu) {
  df <- tibble(
    .rid         = seq_along(quantite),
    quantite     = as.numeric(quantite),
    unite_code   = as.integer(unite_code),
    produit_code = as.integer(produit_code),
    region       = as.integer(region),
    milieu       = as.integer(milieu)
  )
  
  df <- df %>% left_join(bloc_standard, by = "unite_code")
  
  cle_exacte <- bloc_eac %>%
    select(produit_code, unite_code, region, milieu, facteur_exact = facteur_kg)
  df <- df %>% left_join(cle_exacte, by = c("produit_code", "unite_code", "region", "milieu"))
  
  df <- df %>% left_join(table_conv_nat, by = c("produit_code", "unite_code"))
  
  df <- df %>%
    mutate(
      facteur_final = case_when(
        !is.na(facteur_kg)     ~ facteur_kg,
        !is.na(facteur_exact)  ~ facteur_exact,
        !is.na(facteur_kg_nat) ~ facteur_kg_nat,
        TRUE                   ~ NA_real_
      ),
      quantite_kg = quantite * facteur_final
    )
  
  df %>% arrange(.rid) %>% pull(quantite_kg)
}

# 4.5 Application des conversions
s16c <- s16c %>%
  mutate(grappe = as.integer(grappe), menage = as.integer(menage)) %>%
  left_join(geo_menage, by = c("grappe", "menage")) %>%
  mutate(
    recolte_kg = convertir_en_kg(qte_recoltee_uml, uml_recolte, s16cq04, region, milieu)
  )

s16d <- s16d %>%
  mutate(grappe = as.integer(grappe), menage = as.integer(menage)) %>%
  left_join(geo_menage, by = c("grappe", "menage")) %>%
  mutate(
    conso_kg  = convertir_en_kg(qte_conso_uml,  uml_conso,  s16dq01, region, milieu),
    don_kg    = convertir_en_kg(qte_don_uml,    uml_don,    s16dq01, region, milieu),
    vente_kg  = convertir_en_kg(qte_vendue_uml, uml_vente,  s16dq01, region, milieu),
    stock_kg  = convertir_en_kg(qte_stock_uml,  uml_stock,  s16dq01, region, milieu)
  )

# 4.6 Sauvegarde
save_rds(s16c, paths$processed, "s16c_converti.rds")
save_rds(s16d, paths$processed, "s16d_converti.rds")
cat("  s16c_converti :", nrow(s16c), "lignes\n")
cat("  s16d_converti :", nrow(s16d), "lignes\n")

# ------------------------------------------------------------------------------
# 5. Table sécurité alimentaire FIES
# ------------------------------------------------------------------------------

cat("Import de la table FIES...\n")

s08a <- load_dta(paths$raw_menage, "s08a_me_mli2021.dta") %>%
  creer_hhid()

save_rds(s08a, paths$processed, "fies.rds")
cat("  fies :", nrow(s08a), "lignes\n")

# ------------------------------------------------------------------------------
# 6. Table prix
# ------------------------------------------------------------------------------

cat("Import de la table prix...\n")

prix <- load_dta(paths$raw_auxiliaires, "ehcvm_prix_mli2021.dta")
save_rds(prix, paths$processed, "prix.rds")
cat("  prix :", nrow(prix), "lignes\n")

# ------------------------------------------------------------------------------
# 7. Table NSU
# ------------------------------------------------------------------------------

cat("Import de la table NSU...\n")

nsu <- load_dta(paths$raw_auxiliaires, "ehcvm_nsu_mli2021.dta")
save_rds(nsu, paths$processed, "nsu.rds")
cat("  nsu :", nrow(nsu), "lignes\n")

# ------------------------------------------------------------------------------
# 8. Table communauté
# ------------------------------------------------------------------------------

cat("Import des données communautaires...\n")

comm1 <- load_dta(paths$raw_communaute, "s01_co_mli2021.dta")
comm3 <- load_dta(paths$raw_communaute, "s03_co_mli2021.dta")

communaute <- comm1 %>%
  left_join(comm3, by = c("vague", "grappe"))

save_rds(communaute, paths$processed, "communaute.rds")
cat("  communaute :", nrow(communaute), "lignes\n")

# ------------------------------------------------------------------------------
# 9. Message final
# ------------------------------------------------------------------------------

cat("\n============================================================\n")
cat("01_import.R – Import et conversion terminés avec succès.\n")
cat("Tables sauvegardées dans", paths$processed, "\n")
cat("============================================================\n")