# ==============================================================================
# 07_etape5_profilage.R
# Étape 5 – Profilage intégré des ménages autour du mil
# Typologie, sécurité alimentaire (FIES/HDDS), profil socioéconomique.
# ==============================================================================

source(here::here("scripts", "00_setup.R"))

create_dir(here::here("outputs", "eda"))

# ------------------------------------------------------------------------------
# 1. Chargement des données brutes
# ------------------------------------------------------------------------------

s00_raw    <- load_dta(paths$raw_menage, "s00_me_mli2021.dta")
s01_raw    <- load_dta(paths$raw_menage, "s01_me_mli2021.dta")
s06_raw    <- load_dta(paths$raw_menage, "s06_me_mli2021.dta")
s07b_raw   <- load_dta(paths$raw_menage, "s07b_me_mli2021.dta")
s08a_raw   <- load_dta(paths$raw_menage, "s08a_me_mli2021.dta")
s14b_raw   <- load_dta(paths$raw_menage, "s14b_me_mli2021.dta")
s16a_raw   <- load_dta(paths$raw_menage, "s16a_me_mli2021.dta")
s16b_raw   <- load_dta(paths$raw_menage, "s16b_me_mli2021.dta")
s16c_raw   <- load_dta(paths$raw_menage, "s16c_me_mli2021.dta")
s16d_raw   <- load_dta(paths$raw_menage, "s16d_me_mli2021.dta")
s17_raw    <- load_dta(paths$raw_menage, "s17_me_mli2021.dta")
s19_raw    <- load_dta(paths$raw_menage, "s19_me_mli2021.dta")

pond       <- load_dta(paths$raw_auxiliaires, "ehcvm_ponderations_mli2021.dta")
welfare    <- load_dta(paths$raw_auxiliaires, "ehcvm_welfare_mli2021.dta")
conso_agr  <- load_dta(paths$raw_auxiliaires, "ehcvm_conso_mli2021.dta")
nsu_raw    <- load_dta(paths$raw_auxiliaires, "ehcvm_nsu_mli2021.dta")

comm1 <- load_dta(paths$raw_communaute, "s01_co_mli2021.dta")
comm3 <- load_dta(paths$raw_communaute, "s03_co_mli2021.dta")

# ------------------------------------------------------------------------------
# 2. Identifiants, poids, fonctions de conversion
# ------------------------------------------------------------------------------

creer_hhid <- function(df) df %>% mutate(hhid = paste(grappe, menage, vague, sep = "_"))

s00_raw    <- creer_hhid(s00_raw)
s01_raw    <- creer_hhid(s01_raw)
s06_raw    <- creer_hhid(s06_raw)
s07b_raw   <- creer_hhid(s07b_raw)
s08a_raw   <- creer_hhid(s08a_raw)
s14b_raw   <- creer_hhid(s14b_raw)
s16a_raw   <- creer_hhid(s16a_raw)
s16b_raw   <- creer_hhid(s16b_raw)
s16c_raw   <- creer_hhid(s16c_raw)
s16d_raw   <- creer_hhid(s16d_raw)
s17_raw    <- creer_hhid(s17_raw)
s19_raw    <- creer_hhid(s19_raw)

poids_menages <- s00_raw %>%
  select(grappe, menage, vague, hhid) %>%
  left_join(pond, by = c("grappe", "menage")) %>%
  select(hhid, hhweight)

tous_menages <- poids_menages %>% distinct(hhid, hhweight)

region_info <- s00_raw %>%
  select(hhid, grappe, vague, region = s00q01, milieu = s00q04)

# Conversion NSU consommation
nsu_fallback <- nsu_raw %>%
  group_by(produitID, uniteID, tailleID) %>%
  summarise(poids_fallback = median(poids, na.rm = TRUE), .groups = "drop")

convertir_en_kg <- function(qte, unite, taille, strate, produit) {
  kg <- if_else(unite == 100, qte, NA_real_)
  non_std <- unite != 100 & unite != 101
  if (any(non_std)) {
    temp <- data.frame(produit = produit[non_std], unite = unite[non_std],
                       taille = taille[non_std], strate = strate[non_std],
                       qte = qte[non_std], stringsAsFactors = FALSE)
    temp <- suppressWarnings({
      temp %>%
        left_join(nsu_raw, by = c("produit" = "produitID", "unite" = "uniteID",
                                  "taille" = "tailleID", "strate" = "strate")) %>%
        left_join(nsu_fallback, by = c("produit" = "produitID", "unite" = "uniteID",
                                       "taille" = "tailleID")) %>%
        mutate(poids_utilise = if_else(!is.na(poids), poids, poids_fallback),
               kg_conv = qte * poids_utilise / 1000)
    })
    kg[non_std] <- temp$kg_conv
  }
  kg
}

# Variables communautaires
comm_vars <- comm1 %>%
  select(grappe, vague, dist_ville = s01q05, electricite = s01q11) %>%
  mutate(electricite = if_else(electricite == 1, 1L, 0L)) %>%
  left_join(
    comm3 %>% select(grappe, vague, irrigation_comm = s03q17,
                     vulgarisation = s03q16, engrais_comm = s03q12,
                     acces_cooperative_comm = s03q03),
    by = c("grappe", "vague")
  ) %>%
  mutate(
    irrigation_comm = if_else(irrigation_comm == 1, 1L, 0L),
    vulgarisation   = if_else(vulgarisation == 1, 1L, 0L),
    engrais_comm    = if_else(engrais_comm == 1, 1L, 0L),
    acces_cooperative_comm = if_else(acces_cooperative_comm == 1, 1L, 0L)
  )

# ------------------------------------------------------------------------------
# 2 bis. Items FIES — liste explicite (CORRECTIF)
# ------------------------------------------------------------------------------
# Le module s08a contient 11 colonnes qui matchent le pattern "s08aq0" :
# s08aq00 (variable hors-échelle, PAS un item FIES), s08aq01-08 (les 8 items
# officiels), s08aq07a et s08aq08a (sous-questions de fréquence, pas des items
# FIES séparés). On fige donc la liste des 8 items ici, une fois pour toutes,
# pour que l'EDA et le calcul final utilisent strictement la même définition.
items_fies <- paste0("s08aq0", 1:8)   # "s08aq01" ... "s08aq08"

# ==============================================================================
# EXPLORATION DES DONNÉES (EDA)
# ==============================================================================

sink(here::here("outputs", "eda", "eda_etape5.txt"), split = TRUE)
cat("============================================================\n")
cat("EDA – Étape 5 : Profilage intégré des ménages (mil)\n")
cat("Date :", format(Sys.time()), "\n")
cat("============================================================\n\n")

# Typologie
producteurs_mil_eda <- s16a_raw %>%
  filter(s16aq08 == 1) %>%
  distinct(hhid) %>%
  mutate(producteur_mil = 1)

consommateurs_mil_eda <- s07b_raw %>%
  filter(s07bq01 %in% c(7, 14, 15), s07bq02 == 1) %>%
  distinct(hhid) %>%
  mutate(consommateur_mil = 1)

typologie_eda <- tous_menages %>%
  left_join(producteurs_mil_eda, by = "hhid") %>%
  left_join(consommateurs_mil_eda, by = "hhid") %>%
  mutate(
    producteur_mil   = if_else(is.na(producteur_mil), 0L, 1L),
    consommateur_mil = if_else(is.na(consommateur_mil), 0L, 1L),
    groupe = case_when(
      producteur_mil == 1 & consommateur_mil == 1 ~ "Producteur-consommateur",
      producteur_mil == 1 & consommateur_mil == 0 ~ "Producteur uniquement",
      producteur_mil == 0 & consommateur_mil == 1 ~ "Consommateur uniquement",
      TRUE ~ "Ni producteur ni consommateur"
    )
  )

cat("Répartition des ménages (culture principale) :\n")
typologie_eda %>% count(groupe) %>% mutate(pct = n / sum(n) * 100) %>% print()

# FIES — CORRECTIF : sélection explicite des 8 items (plus de starts_with())
fies_eda <- s08a_raw %>%
  select(hhid, all_of(items_fies)) %>%
  mutate(across(all_of(items_fies), ~ if_else(.x == 1, 1L, 0L))) %>%
  rowwise() %>%
  mutate(fies_score = sum(c_across(all_of(items_fies)), na.rm = TRUE)) %>%
  ungroup()

stopifnot(
  "FIES score hors échelle [0,8] détecté (EDA) !" =
    all(fies_eda$fies_score >= 0 & fies_eda$fies_score <= 8, na.rm = TRUE)
)

cat("\nScore FIES (0-8) :\n")
print(summary(fies_eda$fies_score))
cat("Seuils : modéré >= 4, sévère >= 7 (FAO).\n")

# HDDS (mapping par intervalles)
cat("\nScore HDDS (0-12) :\n")

groupe_hdds <- function(p) {
  dplyr::case_when(
    p %in% 1:26                      ~ 1,
    p %in% 123:132                   ~ 2,
    p %in% 88:108                    ~ 3,
    p %in% c(71:87, 133)             ~ 4,
    p %in% 27:39                     ~ 5,
    p == 60                          ~ 6,
    p %in% 40:51                     ~ 7,
    p %in% 109:122                   ~ 8,
    p %in% 52:59                     ~ 9,
    p %in% 61:70                     ~ 10,
    p %in% 134:138                   ~ 11,
    p %in% 139:165                   ~ 12,
    TRUE ~ NA_integer_
  )
}

hdds_eda <- s07b_raw %>%
  filter(s07bq02 == 1) %>%
  mutate(grp = groupe_hdds(as.integer(s07bq01))) %>%
  filter(!is.na(grp)) %>%
  group_by(hhid) %>%
  summarise(hdds_score = n_distinct(grp), .groups = "drop")

print(summary(hdds_eda$hdds_score))

cat("\nEDA terminée.\n")
sink()

# ==============================================================================
# PHASE 2 – TYPOLOGIE, INDICATEURS CLÉS ET PROFILAGE
# ==============================================================================

# --- 1. Typologie ---
producteurs_mil <- s16a_raw %>%
  filter(s16aq08 == 1) %>%
  distinct(hhid) %>%
  mutate(producteur_mil = 1)

consommateurs_mil <- s07b_raw %>%
  filter(s07bq01 %in% c(7, 14, 15), s07bq02 == 1) %>%
  distinct(hhid) %>%
  mutate(consommateur_mil = 1)

typologie <- tous_menages %>%
  left_join(producteurs_mil, by = "hhid") %>%
  left_join(consommateurs_mil, by = "hhid") %>%
  mutate(
    producteur_mil   = if_else(is.na(producteur_mil), 0L, 1L),
    consommateur_mil = if_else(is.na(consommateur_mil), 0L, 1L),
    groupe = case_when(
      producteur_mil == 1 & consommateur_mil == 1 ~ "Producteur-consommateur",
      producteur_mil == 1 & consommateur_mil == 0 ~ "Producteur uniquement",
      producteur_mil == 0 & consommateur_mil == 1 ~ "Consommateur uniquement",
      TRUE ~ "Ni producteur ni consommateur"
    )
  )

# --- 2. Production totale, ventes, autoconsommation (s16d) ---
recolte_mil <- s16c_raw %>%
  filter(s16cq04 == 1) %>%
  mutate(uml = s16cq16a, unite = s16cq16b, kg = s16cq16c) %>%
  filter(uml > 0, kg > 0) %>% select(unite, uml, kg)

ventes_mil_conv <- s16d_raw %>%
  filter(s16dq01 == 1) %>%
  mutate(uml = s16dq05a, unite = s16dq05b, kg = s16dq05c) %>%
  filter(uml > 0, kg > 0) %>% select(unite, uml, kg)

couples_mil <- bind_rows(recolte_mil, ventes_mil_conv) %>%
  mutate(ratio = kg / uml) %>%
  filter(ratio >= quantile(ratio, 0.05, na.rm = TRUE),
         ratio <= quantile(ratio, 0.95, na.rm = TRUE))

conv_prod_mil <- couples_mil %>%
  group_by(unite) %>%
  summarise(ratio_kg_uml = median(ratio, na.rm = TRUE), .groups = "drop") %>%
  bind_rows(data.frame(unite = c(1,5,6,7), ratio_kg_uml = c(1, 25, 50, 100))) %>%
  group_by(unite) %>%
  summarise(ratio_kg_uml = first(na.omit(ratio_kg_uml)), .groups = "drop")

util_mil <- s16d_raw %>%
  filter(s16dq01 == 1) %>%
  pivot_longer(cols = c(s16dq02a, s16dq03a, s16dq05a, s16dq13a),
               names_to = "type_qte", values_to = "uml") %>%
  mutate(unite = case_when(
    type_qte == "s16dq02a" ~ s16dq02b,
    type_qte == "s16dq03a" ~ s16dq03b,
    type_qte == "s16dq05a" ~ s16dq05b,
    type_qte == "s16dq13a" ~ s16dq13b
  )) %>%
  filter(uml > 0, !is.na(uml))

util_conv <- util_mil %>%
  left_join(conv_prod_mil, by = "unite") %>%
  mutate(kg = uml * ratio_kg_uml)

prod_menage <- util_conv %>%
  group_by(hhid) %>%
  summarise(
    prod_totale_kg = sum(kg, na.rm = TRUE),
    vente_kg       = sum(kg[type_qte == "s16dq05a"], na.rm = TRUE),
    .groups = "drop"
  )

# --- 3. Consommation de mil (s07b) avec conversion NSU ---
conso_base <- s07b_raw %>%
  filter(s07bq01 %in% c(7, 14, 15), s07bq02 == 1) %>%
  left_join(region_info, by = "hhid") %>%
  mutate(strate_menage = region * 10 + milieu,
         qte_kg = convertir_en_kg(s07bq03a, s07bq03b, s07bq03c, strate_menage, s07bq01),
         autoconso_kg = convertir_en_kg(s07bq04, s07bq03b, s07bq03c, strate_menage, s07bq01))

conso_menage <- conso_base %>%
  group_by(hhid) %>%
  summarise(
    conso_totale_kg = sum(qte_kg, na.rm = TRUE),
    autoconso_kg    = sum(autoconso_kg, na.rm = TRUE),
    depense_mil     = sum(s07bq08, na.rm = TRUE),
    .groups = "drop"
  )

# --- 4. Score FIES et HDDS ---
# CORRECTIF : sélection explicite des 8 items (plus de starts_with("s08aq0"),
# qui matchait à tort s08aq00, s08aq07a et s08aq08a — voir note plus haut).
fies <- s08a_raw %>%
  select(hhid, all_of(items_fies)) %>%
  mutate(across(all_of(items_fies), ~ if_else(.x == 1, 1L, 0L))) %>%
  rowwise() %>%
  mutate(fies_score = sum(c_across(all_of(items_fies)), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(fies_modere = if_else(fies_score >= 4, 1L, 0L),
         fies_severe = if_else(fies_score >= 7, 1L, 0L))

stopifnot(
  "FIES score hors échelle [0,8] détecté !" =
    all(fies$fies_score >= 0 & fies$fies_score <= 8, na.rm = TRUE)
)

hdds <- s07b_raw %>%
  filter(s07bq02 == 1) %>%
  mutate(grp = groupe_hdds(as.integer(s07bq01))) %>%
  filter(!is.na(grp)) %>%
  group_by(hhid) %>%
  summarise(hdds_score = n_distinct(grp), .groups = "drop")

# --- 5. Variables socioéconomiques ---
chef <- s01_raw %>%
  filter(s01q02 == 1) %>%
  select(hhid, sexe_chef = s01q01, age_chef = s01q04a, educ_chef = s01q25)

welfare <- welfare %>%
  mutate(hhid = paste(grappe, menage, vague, sep = "_")) %>%
  select(hhid, hhsize, pcexp, zref, milieu, region)

superficie_totale <- s16a_raw %>%
  mutate(sup_ha = case_when(
    !is.na(s16aq47) & s16aq47 > 0 ~ s16aq47,
    s16aq09b == 1 ~ s16aq09a,
    s16aq09b == 2 ~ s16aq09a / 10000,
    TRUE ~ NA_real_
  )) %>%
  group_by(hhid) %>%
  summarise(superficie_totale = sum(sup_ha, na.rm = TRUE), .groups = "drop")

cheptel <- s17_raw %>%
  select(hhid, s17q01, s17q03) %>%
  filter(!is.na(s17q03) & s17q03 > 0) %>%
  group_by(hhid) %>%
  summarise(
    nb_bovins        = sum(s17q03[s17q01 == 1], na.rm = TRUE),
    nb_ovins_caprins = sum(s17q03[s17q01 %in% 2:3], na.rm = TRUE),
    nb_volailles     = sum(s17q03[s17q01 %in% 9:11], na.rm = TRUE),
    .groups = "drop"
  )

equipements <- s19_raw %>%
  filter(!is.na(s19q02) & s19q02 > 0) %>%
  group_by(hhid) %>%
  summarise(nb_equipements = sum(s19q02, na.rm = TRUE), .groups = "drop")

intrants_achat <- s16b_raw %>%
  filter(s16bq08 == 1) %>%
  group_by(hhid) %>%
  summarise(valeur_intrants = sum(s16bq09c, na.rm = TRUE), .groups = "drop")

credit <- s06_raw %>%
  filter(!is.na(s06q03) & s06q03 == 1) %>%
  distinct(hhid) %>%
  mutate(acces_credit = 1)

coop <- comm3 %>%
  select(grappe, vague, s03q03) %>%
  mutate(acces_cooperative = if_else(s03q03 == 1, 1L, 0L)) %>%
  left_join(s00_raw %>% select(hhid, grappe, vague), by = c("grappe", "vague")) %>%
  select(hhid, acces_cooperative)

chocs <- s14b_raw %>%
  filter(s14bq02 == 1) %>%
  count(hhid, name = "nb_chocs")

dep_alim <- conso_agr %>%
  filter(coicop == 1) %>%
  group_by(hhid = as.character(hhid)) %>%
  summarise(dep_alim_tot = sum(depan, na.rm = TRUE), .groups = "drop")

dep_mil_agr <- conso_agr %>%
  filter(codpr %in% c(7, 14, 15)) %>%
  group_by(hhid = as.character(hhid)) %>%
  summarise(dep_mil_agr = sum(depan, na.rm = TRUE), .groups = "drop")

part_budget <- full_join(dep_alim, dep_mil_agr, by = "hhid") %>%
  mutate(part_budget_mil = if_else(dep_alim_tot > 0, dep_mil_agr / dep_alim_tot, 0))

# --- 6. Assemblage de la base de profilage ---
base_profil <- typologie %>%
  left_join(fies, by = "hhid") %>%
  left_join(hdds, by = "hhid") %>%
  left_join(prod_menage, by = "hhid") %>%
  left_join(conso_menage, by = "hhid") %>%
  left_join(chef, by = "hhid") %>%
  left_join(welfare %>% select(hhid, hhsize, pcexp, zref), by = "hhid") %>%
  left_join(region_info, by = "hhid") %>%
  left_join(superficie_totale, by = "hhid") %>%
  left_join(cheptel, by = "hhid") %>%
  left_join(equipements, by = "hhid") %>%
  left_join(intrants_achat, by = "hhid") %>%
  left_join(credit, by = "hhid") %>%
  left_join(coop, by = "hhid") %>%
  left_join(chocs, by = "hhid") %>%
  left_join(part_budget, by = "hhid") %>%
  left_join(comm_vars, by = c("grappe", "vague")) %>%
  mutate(
    across(c(prod_totale_kg, vente_kg, conso_totale_kg, autoconso_kg,
             superficie_totale, nb_bovins, nb_ovins_caprins, nb_volailles,
             nb_equipements, valeur_intrants, depense_mil, part_budget_mil),
           ~ if_else(is.na(.x), 0, .x)),
    taux_autoconso = if_else(prod_totale_kg > 0, autoconso_kg / prod_totale_kg, 0),
    taux_commercialisation = if_else(prod_totale_kg > 0, vente_kg / prod_totale_kg, 0),
    pauvre = if_else(pcexp < zref, 1L, 0L),
    acces_credit = if_else(is.na(acces_credit), 0L, acces_credit),
    acces_cooperative = if_else(is.na(acces_cooperative), 0L, acces_cooperative),
    nb_chocs = if_else(is.na(nb_chocs), 0L, nb_chocs),
    across(c(dist_ville, electricite, irrigation_comm, vulgarisation,
             engrais_comm, acces_cooperative_comm),
           ~ if_else(is.na(.x), 0, .x))
  )

# --- 7. Profil comparatif des groupes ---

ind_autoconso <- base_profil %>%
  filter(groupe == "Producteur-consommateur") %>%
  with(weighted.mean(taux_autoconso, hhweight, na.rm = TRUE))

ind_com <- base_profil %>%
  filter(producteur_mil == 1) %>%
  with(weighted.mean(taux_commercialisation, hhweight, na.rm = TRUE))

ind_budget <- base_profil %>%
  filter(groupe == "Consommateur uniquement") %>%
  with(weighted.mean(part_budget_mil, hhweight, na.rm = TRUE))

profil_complet <- base_profil %>%
  group_by(groupe) %>%
  summarise(
    effectif = n(),
    fies_moyen       = round(weighted.mean(fies_score, hhweight, na.rm = TRUE), 2),
    hdds_moyen       = round(weighted.mean(hdds_score, hhweight, na.rm = TRUE), 2),
    fies_modere_pct  = round(weighted.mean(fies_modere, hhweight, na.rm = TRUE) * 100, 1),
    fies_severe_pct  = round(weighted.mean(fies_severe, hhweight, na.rm = TRUE) * 100, 1),
    hhsize_moy       = round(weighted.mean(hhsize, hhweight, na.rm = TRUE), 1),
    pcexp_med        = round(matrixStats::weightedMedian(pcexp, hhweight, na.rm = TRUE), 0),
    pauvre_pct       = round(weighted.mean(pauvre, hhweight, na.rm = TRUE) * 100, 1),
    age_chef_moy     = round(weighted.mean(age_chef, hhweight, na.rm = TRUE), 1),
    sexe_chef_homme_pct = round(weighted.mean(sexe_chef == 1, hhweight, na.rm = TRUE) * 100, 1),
    educ_chef_pct    = round(weighted.mean(educ_chef > 1 & educ_chef != 9999, hhweight, na.rm = TRUE) * 100, 1),
    superf_moy       = round(weighted.mean(superficie_totale, hhweight, na.rm = TRUE), 2),
    bovins_moy       = round(weighted.mean(nb_bovins, hhweight, na.rm = TRUE), 2),
    ovins_moy        = round(weighted.mean(nb_ovins_caprins, hhweight, na.rm = TRUE), 2),
    volailles_moy    = round(weighted.mean(nb_volailles, hhweight, na.rm = TRUE), 1),
    equip_moy        = round(weighted.mean(nb_equipements, hhweight, na.rm = TRUE), 1),
    intrants_moy     = round(weighted.mean(valeur_intrants, hhweight, na.rm = TRUE), 0),
    credit_pct       = round(weighted.mean(acces_credit, hhweight, na.rm = TRUE) * 100, 1),
    coop_pct         = round(weighted.mean(acces_cooperative, hhweight, na.rm = TRUE) * 100, 1),
    chocs_moy        = round(weighted.mean(nb_chocs, hhweight, na.rm = TRUE), 2),
    dist_ville_moy   = round(weighted.mean(dist_ville, hhweight, na.rm = TRUE), 1),
    electricite_pct  = round(weighted.mean(electricite, hhweight, na.rm = TRUE) * 100, 1),
    irrigation_comm_pct = round(weighted.mean(irrigation_comm, hhweight, na.rm = TRUE) * 100, 1),
    vulgarisation_pct   = round(weighted.mean(vulgarisation, hhweight, na.rm = TRUE) * 100, 1),
    engrais_comm_pct    = round(weighted.mean(engrais_comm, hhweight, na.rm = TRUE) * 100, 1),
    coop_comm_pct       = round(weighted.mean(acces_cooperative_comm, hhweight, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(
    taux_autoconso_moy = if_else(groupe == "Producteur-consommateur", round(ind_autoconso * 100, 1), NA_real_),
    taux_com_moy = case_when(
      groupe %in% c("Producteur-consommateur", "Producteur uniquement") ~ round(ind_com * 100, 1),
      TRUE ~ NA_real_
    ),
    part_budget_moy = if_else(groupe == "Consommateur uniquement", round(ind_budget * 100, 2), NA_real_)
  )

print(profil_complet)

# ==============================================================================
# EXPORT EXCEL (plusieurs feuilles thématiques)
# ==============================================================================

wb <- createWorkbook()

titre_style  <- createStyle(fontSize = 14, fontColour = "#2E4053", textDecoration = "bold")
header_style <- createStyle(fontColour = "#ffffff", fgFill = "#4B5320",
                            halign = "center", textDecoration = "bold",
                            border = "TopBottomLeftRight")
body_style   <- createStyle(halign = "left", border = "TopBottomLeftRight")

# --- Feuille 1 : Sécurité alimentaire ---
secu <- profil_complet %>%
  select(groupe, effectif, fies_moyen, hdds_moyen, fies_modere_pct, fies_severe_pct)

addWorksheet(wb, "Sécurité alimentaire")
writeData(wb, "Sécurité alimentaire", "Indicateurs de sécurité alimentaire par groupe", startCol = 1, startRow = 1)
mergeCells(wb, "Sécurité alimentaire", cols = 1:ncol(secu), rows = 1)
addStyle(wb, "Sécurité alimentaire", titre_style, rows = 1, cols = 1:ncol(secu), gridExpand = TRUE)
writeData(wb, "Sécurité alimentaire", secu, startRow = 3)
addStyle(wb, "Sécurité alimentaire", header_style, rows = 3, cols = 1:ncol(secu), gridExpand = TRUE)
addStyle(wb, "Sécurité alimentaire", body_style, rows = 4:(4+nrow(secu)-1), cols = 1:ncol(secu), gridExpand = TRUE)
setColWidths(wb, "Sécurité alimentaire", cols = 1:ncol(secu), widths = "auto")

# --- Feuille 2 : Démographie et capital humain ---
demo <- profil_complet %>%
  select(groupe, hhsize_moy, pcexp_med, pauvre_pct, age_chef_moy, sexe_chef_homme_pct, educ_chef_pct)

addWorksheet(wb, "Démographie")
writeData(wb, "Démographie", "Caractéristiques démographiques par groupe", startCol = 1, startRow = 1)
mergeCells(wb, "Démographie", cols = 1:ncol(demo), rows = 1)
addStyle(wb, "Démographie", titre_style, rows = 1, cols = 1:ncol(demo), gridExpand = TRUE)
writeData(wb, "Démographie", demo, startRow = 3)
addStyle(wb, "Démographie", header_style, rows = 3, cols = 1:ncol(demo), gridExpand = TRUE)
addStyle(wb, "Démographie", body_style, rows = 4:(4+nrow(demo)-1), cols = 1:ncol(demo), gridExpand = TRUE)
setColWidths(wb, "Démographie", cols = 1:ncol(demo), widths = "auto")

# --- Feuille 3 : Actifs agricoles ---
actifs <- profil_complet %>%
  select(groupe, superf_moy, bovins_moy, ovins_moy, volailles_moy, equip_moy, intrants_moy,
         credit_pct, coop_pct, chocs_moy)

addWorksheet(wb, "Actifs agricoles")
writeData(wb, "Actifs agricoles", "Actifs agricoles et accès aux services par groupe", startCol = 1, startRow = 1)
mergeCells(wb, "Actifs agricoles", cols = 1:ncol(actifs), rows = 1)
addStyle(wb, "Actifs agricoles", titre_style, rows = 1, cols = 1:ncol(actifs), gridExpand = TRUE)
writeData(wb, "Actifs agricoles", actifs, startRow = 3)
addStyle(wb, "Actifs agricoles", header_style, rows = 3, cols = 1:ncol(actifs), gridExpand = TRUE)
addStyle(wb, "Actifs agricoles", body_style, rows = 4:(4+nrow(actifs)-1), cols = 1:ncol(actifs), gridExpand = TRUE)
setColWidths(wb, "Actifs agricoles", cols = 1:ncol(actifs), widths = "auto")

# --- Feuille 4 : Contexte communautaire ---
comm_excel <- profil_complet %>%
  select(groupe, dist_ville_moy, electricite_pct, irrigation_comm_pct,
         vulgarisation_pct, engrais_comm_pct, coop_comm_pct)

addWorksheet(wb, "Contexte communautaire")
writeData(wb, "Contexte communautaire", "Variables communautaires par groupe", startCol = 1, startRow = 1)
mergeCells(wb, "Contexte communautaire", cols = 1:ncol(comm_excel), rows = 1)
addStyle(wb, "Contexte communautaire", titre_style, rows = 1, cols = 1:ncol(comm_excel), gridExpand = TRUE)
writeData(wb, "Contexte communautaire", comm_excel, startRow = 3)
addStyle(wb, "Contexte communautaire", header_style, rows = 3, cols = 1:ncol(comm_excel), gridExpand = TRUE)
addStyle(wb, "Contexte communautaire", body_style, rows = 4:(4+nrow(comm_excel)-1), cols = 1:ncol(comm_excel), gridExpand = TRUE)
setColWidths(wb, "Contexte communautaire", cols = 1:ncol(comm_excel), widths = "auto")

# --- Feuille 5 : Indicateurs spécifiques mil ---
spec <- profil_complet %>%
  select(groupe, taux_autoconso_moy, taux_com_moy, part_budget_moy) %>%
  filter(!is.na(taux_autoconso_moy) | !is.na(taux_com_moy) | !is.na(part_budget_moy))

addWorksheet(wb, "Spécifiques mil")
writeData(wb, "Spécifiques mil", "Indicateurs spécifiques à la filière mil", startCol = 1, startRow = 1)
mergeCells(wb, "Spécifiques mil", cols = 1:ncol(spec), rows = 1)
addStyle(wb, "Spécifiques mil", titre_style, rows = 1, cols = 1:ncol(spec), gridExpand = TRUE)
writeData(wb, "Spécifiques mil", spec, startRow = 3)
addStyle(wb, "Spécifiques mil", header_style, rows = 3, cols = 1:ncol(spec), gridExpand = TRUE)
addStyle(wb, "Spécifiques mil", body_style, rows = 4:(4+nrow(spec)-1), cols = 1:ncol(spec), gridExpand = TRUE)
setColWidths(wb, "Spécifiques mil", cols = 1:ncol(spec), widths = "auto")

# --- Feuille 6 : Dictionnaire ---
dico <- data.frame(
  Indicateur = c("fies_moyen","hdds_moyen","fies_modere_pct","fies_severe_pct",
                 "hhsize_moy","pcexp_med","pauvre_pct",
                 "age_chef_moy","sexe_chef_homme_pct","educ_chef_pct",
                 "superf_moy","bovins_moy","ovins_moy","volailles_moy","equip_moy",
                 "intrants_moy","credit_pct","coop_pct","chocs_moy",
                 "dist_ville_moy","electricite_pct","irrigation_comm_pct",
                 "vulgarisation_pct","engrais_comm_pct","coop_comm_pct",
                 "taux_autoconso_moy","taux_com_moy","part_budget_moy"),
  Description = c(
    "Score moyen d'insécurité alimentaire (FIES, 0-8)",
    "Score moyen de diversité alimentaire (HDDS, 0-12)",
    "Proportion de ménages en insécurité modérée (FIES >= 4, seuil FAO) (%)",
    "Proportion de ménages en insécurité sévère (FIES >= 7, seuil FAO) (%)",
    "Taille moyenne du ménage",
    "Dépense médiane par tête (FCFA)",
    "Proportion de ménages pauvres (dépense/tête < seuil national) (%)",
    "Âge moyen du chef de ménage (années)",
    "Proportion de chefs de ménage de sexe masculin (%)",
    "Proportion de chefs de ménage ayant au moins le niveau primaire (%)",
    "Superficie agricole totale moyenne par ménage (ha)",
    "Nombre moyen de bovins par ménage",
    "Nombre moyen d'ovins/caprins par ménage",
    "Nombre moyen de volailles par ménage",
    "Nombre moyen d'équipements agricoles par ménage",
    "Valeur moyenne des intrants achetés par ménage (FCFA)",
    "Proportion de ménages ayant accès au crédit (%)",
    "Proportion de ménages dont la communauté possède une coopérative (%)",
    "Nombre moyen de chocs subis par ménage",
    "Distance moyenne à la ville la plus proche (km)",
    "Proportion de ménages vivant dans une communauté électrifiée (%)",
    "Proportion de ménages vivant dans une communauté pratiquant l'irrigation (%)",
    "Proportion de ménages dont la communauté bénéficie d'agents de vulgarisation (%)",
    "Proportion de ménages dont la communauté utilise des engrais chimiques (%)",
    "Proportion de ménages dont la communauté possède une coopérative (%)",
    "Taux d'autoconsommation moyen (producteurs-consommateurs) (%)",
    "Taux de commercialisation moyen (producteurs) (%)",
    "Part budgétaire moyenne du mil (consommateurs uniquement) (%)"
  )
)

addWorksheet(wb, "Dictionnaire")
writeData(wb, "Dictionnaire", "Dictionnaire des indicateurs", startCol = 1, startRow = 1)
mergeCells(wb, "Dictionnaire", cols = 1:2, rows = 1)
addStyle(wb, "Dictionnaire", titre_style, rows = 1, cols = 1:2, gridExpand = TRUE)
writeData(wb, "Dictionnaire", dico, startRow = 3)
addStyle(wb, "Dictionnaire", header_style, rows = 3, cols = 1:2, gridExpand = TRUE)
addStyle(wb, "Dictionnaire", body_style, rows = 4:(4+nrow(dico)-1), cols = 1:2, gridExpand = TRUE)
setColWidths(wb, "Dictionnaire", cols = 1:2, widths = "auto")

saveWorkbook(wb, here::here("outputs", "tables", "resultats_etape5.xlsx"), overwrite = TRUE)

# ==============================================================================
# THÈME GRAPHIQUE (définition locale)
# ==============================================================================

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

# ==============================================================================
# VISUALISATION (un seul graphique : FIES par groupe)
# ==============================================================================

p_fies <- ggplot(base_profil, aes(x = groupe, y = fies_score, fill = groupe)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Score FIES (0-8)", x = "",
       title = "Insécurité alimentaire (FIES) selon le statut vis-à-vis du mil",
       caption = "Source : EHCVM-2 Mali 2021/22. Seuils FAO : modéré >= 4, sévère >= 7.") +
  theme_filiere

ggsave(here::here("outputs", "figures", "fies_par_groupe.png"),
       p_fies, width = 8, height = 5)

# ==============================================================================
# SAUVEGARDE DE LA BASE TYPOLOGIQUE
# ==============================================================================

saveRDS(base_profil, here::here("data", "processed", "typologie_mil.rds"))

cat("\nÉtape 5 terminée. Base typologique sauvegardée dans data/processed/typologie_mil.rds\n")