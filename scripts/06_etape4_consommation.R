# ==============================================================================
# 06_etape4_consommation.R
# Étape 4 – Consommation et demande de mil
# EDA, indicateurs, substitution, acheteurs nets, graphiques
# ==============================================================================

source(here::here("scripts", "00_setup.R"))

create_dir(here::here("outputs", "eda"))
create_dir(here::here("outputs", "figures"))

# ------------------------------------------------------------------------------
# 1. Chargement des données
# ------------------------------------------------------------------------------

s00_raw   <- load_dta(paths$raw_menage, "s00_me_mli2021.dta")
s07b_raw  <- load_dta(paths$raw_menage, "s07b_me_mli2021.dta")
s16d_raw  <- load_dta(paths$raw_menage, "s16d_me_mli2021.dta")
conso_agr <- load_dta(paths$raw_auxiliaires, "ehcvm_conso_mli2021.dta")
nsu_raw   <- load_dta(paths$raw_auxiliaires, "ehcvm_nsu_mli2021.dta")
pond      <- load_dta(paths$raw_auxiliaires, "ehcvm_ponderations_mli2021.dta")
comm1     <- load_dta(paths$raw_communaute, "s01_co_mli2021.dta")
comm2     <- load_dta(paths$raw_communaute, "s02_co_mli2021.dta")
welfare   <- load_dta(paths$raw_auxiliaires, "ehcvm_welfare_mli2021.dta")

# ------------------------------------------------------------------------------
# 2. Identifiants, poids, géolocalisation
# ------------------------------------------------------------------------------

creer_hhid <- function(df) df %>% mutate(hhid = paste(grappe, menage, vague, sep = "_"))

s00_raw  <- creer_hhid(s00_raw)
s07b_raw <- creer_hhid(s07b_raw) %>% select(-grappe, -menage, -vague)
s16d_raw <- creer_hhid(s16d_raw)

poids_menages <- s00_raw %>%
  select(grappe, menage, vague, hhid) %>%
  left_join(pond, by = c("grappe", "menage")) %>%
  select(hhid, hhweight)

tous_menages <- poids_menages %>% distinct(hhid, hhweight)

menage_geo <- s00_raw %>%
  select(hhid, grappe, menage, vague, region = s00q01, milieu = s00q04)

codes_mil_conso <- c(7, 14, 15)
codes_cereales <- list(
  mil = c(7,14,15), riz = c(1,2,3,4,19),
  mais = c(5,6,12,13), sorgho = c(8)
)

# ------------------------------------------------------------------------------
# 3. Variables communautaires
# ------------------------------------------------------------------------------

comm_vars <- comm1 %>%
  select(grappe, vague, dist_ville = s01q05) %>%
  left_join(
    comm2 %>%
      filter(s02q00 == 10) %>%
      group_by(grappe, vague) %>%
      summarise(temps_marche = if(all(is.na(s02q03))) NA_real_ else min(s02q03, na.rm = TRUE),
                .groups = "drop"),
    by = c("grappe", "vague")
  )

# ------------------------------------------------------------------------------
# 4. Conversion NSU et création de s07b_converti
# ------------------------------------------------------------------------------

nsu_fallback <- nsu_raw %>%
  group_by(produitID, uniteID, tailleID) %>%
  summarise(poids_fallback = median(poids, na.rm = TRUE), .groups = "drop")

convertir_en_kg <- function(qte, unite, taille, strate, produit) {
  n <- length(qte)
  kg <- rep(NA_real_, n)
  std <- unite == 100 & !is.na(unite)
  kg[std] <- qte[std]
  non_std <- which(!std & !is.na(qte) & qte > 0)
  if (length(non_std) == 0) return(kg)
  temp <- data.frame(produit = produit[non_std], unite = unite[non_std],
                     taille = taille[non_std], strate = strate[non_std],
                     qte = qte[non_std])
  temp <- temp %>%
    left_join(nsu_raw, by = c("produit" = "produitID", "unite" = "uniteID",
                              "taille" = "tailleID", "strate" = "strate")) %>%
    left_join(nsu_fallback, by = c("produit" = "produitID", "unite" = "uniteID",
                                   "taille" = "tailleID")) %>%
    mutate(poids_utilise = if_else(!is.na(poids), poids, poids_fallback),
           kg_conv = qte * poids_utilise / 1000)
  kg[non_std] <- temp$kg_conv
  kg
}

s07b_conv <- s07b_raw %>%
  filter(s07bq02 == 1) %>%
  left_join(menage_geo, by = "hhid") %>%
  mutate(
    strate_menage = region * 10 + milieu,
    produit = as.integer(s07bq01),
    quantite_kg = convertir_en_kg(qte = s07bq03a, unite = s07bq03b, taille = s07bq03c,
                                  strate = strate_menage, produit = produit),
    autoconso_kg = convertir_en_kg(qte = s07bq04, unite = s07bq03b, taille = s07bq03c,
                                   strate = strate_menage, produit = produit)
  )

saveRDS(s07b_conv, here::here("data", "processed", "s07b_converti.rds"))

# ==============================================================================
# PHASE 1 – EDA
# ==============================================================================

sink(here::here("outputs", "eda", "eda_etape4.txt"), split = TRUE)
cat("============================================================\n")
cat("EDA – Étape 4 : Consommation et demande de mil\n")
cat("Date :", format(Sys.time()), "\n")
cat("============================================================\n\n")

cat("1. Présence et modes d'acquisition\n")
cat("------------------------------------\n")
conso_mil_eda <- s07b_conv %>% filter(produit %in% codes_mil_conso)
cat(sprintf("Nombre de lignes de consommation (mil, farine, semoule) : %d\n", nrow(conso_mil_eda)))
cat("Modes d'acquisition (s07bq06) :\n")
print(table(conso_mil_eda$s07bq06, useNA = "ifany"))
cat("1=Acheté, 2=Autoconsommé, 3=Reçu en cadeau, 4=Prélèvement propre commerce, 5=Troc\n\n")

cat("2. Quantités déclarées (kg)\n")
cat("-----------------------------\n")
cat(sprintf("Quantités (kg) : min=%.2f  Q1=%.2f  médiane=%.2f  Q3=%.2f  max=%.2f\n",
            min(conso_mil_eda$quantite_kg, na.rm=TRUE),
            quantile(conso_mil_eda$quantite_kg, 0.25, na.rm=TRUE),
            median(conso_mil_eda$quantite_kg, na.rm=TRUE),
            quantile(conso_mil_eda$quantite_kg, 0.75, na.rm=TRUE),
            max(conso_mil_eda$quantite_kg, na.rm=TRUE)))
cat("Décision : conversion NSU avec table EHCVM, fallback médian national.\n\n")

cat("3. Prévalence de consommation\n")
cat("------------------------------\n")
conso_poids <- s07b_conv %>% left_join(tous_menages, by = "hhid")
nb_cons_pond <- conso_poids %>%
  filter(produit %in% codes_mil_conso) %>%
  distinct(hhid, hhweight) %>% pull(hhweight) %>% sum()
nb_total_pond <- sum(tous_menages$hhweight)
prev_cons <- nb_cons_pond / nb_total_pond * 100
cat(sprintf("Prévalence pondérée : %.1f%%\n", prev_cons))
cat("Décision : indicateur calculé en pondérant par hhweight.\n\n")

cat("4. Achats récents\n")
cat("------------------\n")
achats_eda <- s07b_conv %>%
  filter(produit %in% codes_mil_conso, s07bq06 %in% 1:3, s07bq07a > 0, s07bq08 > 0)
cat(sprintf("Nombre de lignes d'achats récents : %d\n", nrow(achats_eda)))
cat(sprintf("Médiane du prix d'achat brut (FCFA/unité) : %.0f\n", median(achats_eda$s07bq08, na.rm=TRUE)))

cat("\n5. Exploration communautaire\n")
cat("-----------------------------\n")
conso_comm_eda <- tous_menages %>%
  left_join(
    s07b_conv %>% filter(produit %in% codes_mil_conso) %>%
      group_by(hhid) %>% summarise(qte_kg_est = sum(quantite_kg, na.rm=TRUE), .groups="drop"),
    by = "hhid") %>%
  left_join(menage_geo, by = "hhid") %>%
  left_join(comm_vars, by = c("grappe","vague")) %>%
  mutate(qte_kg_est = if_else(is.na(qte_kg_est), 0, qte_kg_est))

cat("Consommation selon la distance à la ville :\n")
conso_comm_eda %>%
  mutate(dist_classe = cut(dist_ville, breaks = c(0,5,15,50,Inf),
                           labels = c("<5 km","5-15 km","15-50 km",">50 km"))) %>%
  group_by(dist_classe) %>%
  summarise(qte_moy = weighted.mean(qte_kg_est, hhweight, na.rm=TRUE), .groups="drop") %>% print()

cat("\nConsommation selon l'accès au marché :\n")
conso_comm_eda %>%
  mutate(marche_classe = cut(temps_marche, breaks = c(0,30,60,120,Inf),
                             labels = c("<30 min","30-60 min","60-120 min",">120 min"))) %>%
  group_by(marche_classe) %>%
  summarise(qte_moy = weighted.mean(qte_kg_est, hhweight, na.rm=TRUE), .groups="drop") %>% print()

cat("\nEDA terminée.\n")
sink()

# ==============================================================================
# PHASE 2 – Indicateurs de consommation du mil
# ==============================================================================

conso_mil <- s07b_conv %>% filter(produit %in% codes_mil_conso)

conso_menage <- conso_mil %>%
  group_by(hhid) %>%
  summarise(
    qte_totale_kg = sum(quantite_kg, na.rm = TRUE),
    autoconso_kg  = sum(autoconso_kg, na.rm = TRUE),
    depense_achat = sum(s07bq08, na.rm = TRUE),
    nb_achats     = sum(s07bq06 %in% 1:3 & !is.na(s07bq08)),
    .groups = "drop"
  ) %>%
  left_join(poids_menages, by = "hhid")

tous_menages_conso <- tous_menages %>%
  left_join(conso_menage %>% select(hhid, qte_totale_kg, autoconso_kg, depense_achat, nb_achats), by = "hhid") %>%
  left_join(menage_geo, by = "hhid") %>%
  left_join(comm_vars, by = c("grappe","vague")) %>%
  mutate(
    qte_totale_kg = if_else(is.na(qte_totale_kg), 0, qte_totale_kg),
    autoconso_kg  = if_else(is.na(autoconso_kg), 0, autoconso_kg),
    depense_achat = if_else(is.na(depense_achat), 0, depense_achat),
    nb_achats     = if_else(is.na(nb_achats), 0L, nb_achats)
  )

cat("\n--- Indicateurs de consommation ---\n")
qte_moyenne <- weighted.mean(tous_menages_conso$qte_totale_kg, tous_menages_conso$hhweight)
cat(sprintf("Quantité moyenne hebdomadaire par ménage : %.2f kg\n", qte_moyenne))
cat(sprintf("Part des ménages consommateurs : %.1f%%\n",
            sum(tous_menages_conso$qte_totale_kg > 0) / nrow(tous_menages_conso) * 100))
part_consommateurs <- sum(tous_menages_conso$qte_totale_kg > 0) / nrow(tous_menages_conso) * 100

conso_positif <- tous_menages_conso %>% filter(qte_totale_kg > 0)
# CORRECTION : part d'autoconsommation = ratio des totaux pondérés (plus robuste)
total_autoconso_pond <- sum(conso_positif$autoconso_kg * conso_positif$hhweight, na.rm = TRUE)
total_conso_pond     <- sum(conso_positif$qte_totale_kg * conso_positif$hhweight, na.rm = TRUE)
part_autoconso_globale <- (total_autoconso_pond / total_conso_pond) * 100
cat(sprintf("Part de l'autoconsommation dans la consommation totale : %.1f%%\n", part_autoconso_globale))

part_acheteurs <- sum(conso_positif$depense_achat > 0) / nrow(conso_positif) * 100
cat(sprintf("Part des consommateurs qui achètent du mil : %.1f%%\n", part_acheteurs))

depense_moyenne <- weighted.mean(conso_positif$depense_achat, conso_positif$hhweight)
cat(sprintf("Dépense hebdomadaire moyenne par ménage consommateur : %.0f FCFA\n", depense_moyenne))

# Part budgétaire
dep_alim <- conso_agr %>%
  filter(coicop == 1) %>%
  group_by(grappe, menage, vague) %>%
  summarise(dep_tot = sum(depan, na.rm = TRUE), .groups = "drop") %>%
  left_join(s00_raw %>% select(grappe, menage, vague, hhid), by = c("grappe","menage","vague")) %>%
  left_join(poids_menages, by = "hhid")
total_dep_alim <- sum(dep_alim$dep_tot * dep_alim$hhweight, na.rm = TRUE)

dep_mil <- conso_agr %>%
  filter(codpr %in% codes_mil_conso) %>%
  group_by(grappe, menage, vague) %>%
  summarise(dep_mil = sum(depan, na.rm = TRUE), .groups = "drop") %>%
  left_join(s00_raw %>% select(grappe, menage, vague, hhid), by = c("grappe","menage","vague")) %>%
  left_join(poids_menages, by = "hhid")
total_dep_mil <- sum(dep_mil$dep_mil * dep_mil$hhweight, na.rm = TRUE)
part_budget_mil <- total_dep_mil / total_dep_alim * 100
cat(sprintf("Part du mil dans la dépense alimentaire : %.2f%%\n", part_budget_mil))

# Prix à la consommation
achats_mil <- s07b_raw %>%
  filter(s07bq01 %in% codes_mil_conso, s07bq06 %in% 1:3, s07bq07a > 0, s07bq08 > 0) %>%
  left_join(menage_geo, by = "hhid") %>%
  mutate(
    strate_menage = region * 10 + milieu,
    qte_achetee_kg = convertir_en_kg(qte = s07bq07a, unite = s07bq07b, taille = s07bq07c,
                                     strate = strate_menage, produit = as.integer(s07bq01))
  ) %>%
  filter(!is.na(qte_achetee_kg)) %>%
  mutate(prix_kg = s07bq08 / qte_achetee_kg) %>%
  left_join(tous_menages, by = "hhid")

prix_conso_median <- median(achats_mil$prix_kg, na.rm = TRUE)
prix_conso_moyen <- weighted.mean(achats_mil$prix_kg, achats_mil$qte_achetee_kg * achats_mil$hhweight, na.rm = TRUE)
cat(sprintf("Prix médian à la consommation : %.0f FCFA/kg\n", prix_conso_median))
cat(sprintf("Prix moyen pondéré à la consommation : %.0f FCFA/kg\n", prix_conso_moyen))

# ==============================================================================
# PHASE 3 – Substitution entre céréales
# ==============================================================================

cereales_kg <- s07b_conv %>%
  mutate(
    cereale = case_when(
      produit %in% codes_cereales$mil    ~ "mil",
      produit %in% codes_cereales$riz    ~ "riz",
      produit %in% codes_cereales$mais   ~ "mais",
      produit %in% codes_cereales$sorgho ~ "sorgho",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(cereale), !is.na(quantite_kg)) %>%
  group_by(hhid, cereale) %>%
  summarise(kg = sum(quantite_kg, na.rm = TRUE), .groups = "drop")

cereale_large <- cereales_kg %>%
  pivot_wider(names_from = cereale, values_from = kg, values_fill = 0) %>%
  mutate(total_cereales = mil + riz + mais + sorgho) %>%
  filter(total_cereales > 0)

welfare <- welfare %>%
  mutate(hhid = paste(grappe, menage, vague, sep = "_")) %>%
  select(hhid, pcexp, milieu, region, hhweight)

base_sub <- cereale_large %>%
  inner_join(welfare, by = "hhid") %>%
  mutate(quintile = ntile(pcexp, 5))

parts_par_groupe <- function(df, groupe) {
  df %>%
    group_by(across(all_of(groupe))) %>%
    summarise(
      n = n(),
      part_mil    = 100 * weighted.mean(mil/total_cereales, hhweight, na.rm=TRUE),
      part_riz    = 100 * weighted.mean(riz/total_cereales, hhweight, na.rm=TRUE),
      part_mais   = 100 * weighted.mean(mais/total_cereales, hhweight, na.rm=TRUE),
      part_sorgho = 100 * weighted.mean(sorgho/total_cereales, hhweight, na.rm=TRUE),
      .groups = "drop"
    )
}

sub_quintile <- parts_par_groupe(base_sub, "quintile") %>% mutate(across(starts_with("part"), ~round(.x,1)))
sub_milieu   <- parts_par_groupe(base_sub, "milieu")   %>% mutate(across(starts_with("part"), ~round(.x,1)))
sub_region   <- parts_par_groupe(base_sub, "region")   %>% mutate(across(starts_with("part"), ~round(.x,1)))

cat("\n--- Substitution par niveau de vie ---\n"); print(sub_quintile)
cat("\n--- Substitution par milieu ---\n"); print(sub_milieu)
cat("\n--- Substitution par région ---\n"); print(sub_region)

# ==============================================================================
# PHASE 4 – Acheteurs nets
# ==============================================================================

producteurs_mil <- s16d_raw %>%
  filter(s16dq01 == 1) %>% distinct(hhid) %>% mutate(producteur = 1)

acheteurs_nets <- tous_menages_conso %>%
  left_join(producteurs_mil, by = "hhid") %>%
  mutate(
    acheteur_net = case_when(
      depense_achat > 0 & (is.na(producteur) | qte_totale_kg > autoconso_kg) ~ 1,
      depense_achat > 0 & is.na(producteur) ~ 1,
      TRUE ~ 0
    )
  )

nb_ach_nets <- sum(acheteurs_nets$acheteur_net)
pct_ach_nets <- nb_ach_nets / nrow(acheteurs_nets) * 100
cat(sprintf("\nNombre de ménages acheteurs nets : %d (%.1f%%)\n", nb_ach_nets, pct_ach_nets))

# ==============================================================================
# EXPORT EXCEL UNIQUE
# ==============================================================================

wb <- createWorkbook()
titre_style  <- createStyle(fontSize = 14, fontColour = "#2E4053", textDecoration = "bold")
header_style <- createStyle(fontColour = "#ffffff", fgFill = "#4B5320", halign = "center",
                            textDecoration = "bold", border = "TopBottomLeftRight")
body_style   <- createStyle(halign = "left", border = "TopBottomLeftRight")

# Feuille 1 : Indicateurs
indicateurs <- data.frame(
  Indicateur = c("Quantité moyenne hebdomadaire","Ménages consommateurs",
                 "Part autoconsommation (ratio global)","Acheteurs (parmi les conso.)",
                 "Dépense moyenne hebdomadaire","Part budgétaire du mil",
                 "Prix médian à la consommation","Prix moyen pondéré à la consommation",
                 "Acheteurs nets"),
  Valeur = c(round(qte_moyenne,2), round(part_consommateurs,1),
             round(part_autoconso_globale,1), round(part_acheteurs,1),
             round(depense_moyenne,0), round(part_budget_mil,2),
             round(prix_conso_median,0), round(prix_conso_moyen,0),
             round(pct_ach_nets,1)),
  Unité = c("kg/semaine","%","%","%","FCFA/semaine","%","FCFA/kg","FCFA/kg","%")
)
addWorksheet(wb, "Indicateurs")
writeData(wb, "Indicateurs", "Indicateurs de consommation du mil", startCol=1, startRow=1)
mergeCells(wb, "Indicateurs", cols=1:3, rows=1)
addStyle(wb, "Indicateurs", titre_style, rows=1, cols=1:3, gridExpand=TRUE)
writeData(wb, "Indicateurs", indicateurs, startRow=3)
addStyle(wb, "Indicateurs", header_style, rows=3, cols=1:3, gridExpand=TRUE)
addStyle(wb, "Indicateurs", body_style, rows=4:(4+nrow(indicateurs)-1), cols=1:3, gridExpand=TRUE)
setColWidths(wb, "Indicateurs", cols=1:3, widths="auto")

# Feuilles 2-4 : Substitution
for (nom_feuille in c("quintile","milieu","region")) {
  data_export <- switch(nom_feuille, quintile=sub_quintile, milieu=sub_milieu, region=sub_region)
  titre <- paste("Part de chaque céréale selon le", 
                 ifelse(nom_feuille=="quintile","niveau de vie",
                        ifelse(nom_feuille=="milieu","milieu de résidence","région")))
  addWorksheet(wb, paste("Substitution par", nom_feuille))
  writeData(wb, paste("Substitution par", nom_feuille), titre, startCol=1, startRow=1)
  mergeCells(wb, paste("Substitution par", nom_feuille), cols=1:6, rows=1)
  addStyle(wb, paste("Substitution par", nom_feuille), titre_style, rows=1, cols=1:6, gridExpand=TRUE)
  writeData(wb, paste("Substitution par", nom_feuille), data_export, startRow=3)
  addStyle(wb, paste("Substitution par", nom_feuille), header_style, rows=3, cols=1:6, gridExpand=TRUE)
  addStyle(wb, paste("Substitution par", nom_feuille), body_style, rows=4:(4+nrow(data_export)-1), cols=1:6, gridExpand=TRUE)
  setColWidths(wb, paste("Substitution par", nom_feuille), cols=1:6, widths="auto")
}

# Feuille 5 : Acheteurs nets par région
ach_nets_region <- acheteurs_nets %>%
  group_by(region) %>%
  summarise(nb_total=n(), nb_acheteurs=sum(acheteur_net),
            pct_acheteurs=round(mean(acheteur_net)*100,1), .groups="drop")
addWorksheet(wb, "Acheteurs nets par région")
writeData(wb, "Acheteurs nets par région", "Acheteurs nets de mil par région", startCol=1, startRow=1)
mergeCells(wb, "Acheteurs nets par région", cols=1:4, rows=1)
addStyle(wb, "Acheteurs nets par région", titre_style, rows=1, cols=1:4, gridExpand=TRUE)
writeData(wb, "Acheteurs nets par région", ach_nets_region, startRow=3)
addStyle(wb, "Acheteurs nets par région", header_style, rows=3, cols=1:4, gridExpand=TRUE)
addStyle(wb, "Acheteurs nets par région", body_style, rows=4:(4+nrow(ach_nets_region)-1), cols=1:4, gridExpand=TRUE)
setColWidths(wb, "Acheteurs nets par région", cols=1:4, widths="auto")

# Feuille 6 : Dictionnaire
addWorksheet(wb, "Dictionnaire")
writeData(wb, "Dictionnaire", "Dictionnaire des indicateurs", startCol=1, startRow=1)
mergeCells(wb, "Dictionnaire", cols=1:2, rows=1)
addStyle(wb, "Dictionnaire", titre_style, rows=1, cols=1:2, gridExpand=TRUE)
dico <- data.frame(
  Indicateur = indicateurs$Indicateur,
  Description = c(
    "Moyenne pondérée des quantités totales de mil consommées par ménage sur 7 jours.",
    "Proportion pondérée de ménages ayant déclaré une consommation de mil au cours des 7 derniers jours.",
    "Part de la quantité totale consommée provenant de l'autoconsommation (ratio des sommes pondérées).",
    "Part des ménages consommateurs ayant effectué un achat de mil dans les 7 jours.",
    "Dépense moyenne hebdomadaire en mil par ménage consommateur, pondérée par les poids d'enquête.",
    "Part des dépenses de mil dans la dépense alimentaire totale, calculée sur l'agrégat de consommation.",
    "Médiane des prix unitaires payés par les ménages pour leurs achats récents de mil.",
    "Moyenne pondérée des prix unitaires, avec poids = quantités achetées * poids d'enquête.",
    "Proportion de ménages dont la consommation de mil dépasse l'autoconsommation."
  )
)
writeData(wb, "Dictionnaire", dico, startRow=3)
addStyle(wb, "Dictionnaire", header_style, rows=3, cols=1:2, gridExpand=TRUE)
addStyle(wb, "Dictionnaire", body_style, rows=4:(4+nrow(dico)-1), cols=1:2, gridExpand=TRUE)
setColWidths(wb, "Dictionnaire", cols=1:2, widths="auto")

saveWorkbook(wb, here::here("outputs", "tables", "resultats_etape4.xlsx"), overwrite = TRUE)

# ==============================================================================
# GRAPHIQUES
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

p1 <- ggplot(conso_positif, aes(x = qte_totale_kg)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  scale_x_log10() +
  labs(x = "Quantité consommée (kg/semaine, échelle log)", y = "Nombre de ménages",
       title = "Distribution des quantités de mil consommées par ménage") +
  theme_filiere
ggsave(here::here("outputs", "figures", "distribution_conso_mil.png"), p1, width = 8, height = 5)

sub_q_long <- sub_quintile %>%
  pivot_longer(cols = starts_with("part_"), names_to = "cereale", values_to = "part") %>%
  mutate(cereale = recode(cereale, part_mil = "Mil", part_riz = "Riz", part_mais = "Maïs", part_sorgho = "Sorgho"))

p2 <- ggplot(sub_q_long, aes(x = factor(quintile), y = part, fill = cereale)) +
  geom_col(position = "fill", width = 0.7) +
  scale_fill_manual(values = c("Mil" = "#2c6e49", "Riz" = "#1d3557", "Maïs" = "#f4a261", "Sorgho" = "#e9c46a")) +
  labs(x = "Quintile de dépense par tête", y = "Part dans la consommation de céréales",
       title = "Substitution entre céréales selon le niveau de vie", fill = NULL) +
  theme_filiere
ggsave(here::here("outputs", "figures", "substitution_cereales_quintile.png"), p2, width = 8, height = 5)

cat("\nÉtape 4 terminée.\n")