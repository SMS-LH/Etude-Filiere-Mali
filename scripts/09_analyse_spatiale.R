# ==============================================================================
# 09_analyse_spatiale.R
# Analyse spatiale et climatique – Précipitations (CHIRPS) et Températures (ERA5)
# ==============================================================================

source(here::here("scripts", "00_setup.R"))

cat("Analyse spatiale et climatique pour la filière mil\n")

# ------------------------------------------------------------------------------
# 1. Chargement des données pré-extraites (légères) et du fond de carte
# ------------------------------------------------------------------------------

climat_menages_path <- here::here("data", "raw", "climat", "climat_menages_mil.rds")
precip_carte_path   <- here::here("data", "raw", "climat", "precip_mali_2021_total.tif")

if (!file.exists(climat_menages_path)) {
  stop("Fichier introuvable : ", climat_menages_path,
       " — lance d'abord scripts/local_only/extraction_climat_chirps_era5.R en local.")
}
if (!file.exists(precip_carte_path)) {
  stop("Fichier introuvable : ", precip_carte_path,
       " — lance d'abord scripts/local_only/extraction_climat_chirps_era5.R en local.")
}

climat_menages <- readRDS(climat_menages_path)
chirps_mali    <- rast(precip_carte_path)

mali_regions <- st_read(here::here("data", "raw", "spatial", "gadm", "gadm41_MLI_1.shp"),
                        quiet = TRUE)

# ------------------------------------------------------------------------------
# 2. Données des ménages (rendements) – version corrigée
# ------------------------------------------------------------------------------

base_profil <- load_rds(paths$processed, "typologie_mil.rds")

base_rdt <- base_profil %>%
  filter(producteur_mil == 1, superficie_totale > 0, prod_totale_kg > 0) %>%
  mutate(
    rdt_brut = prod_totale_kg / superficie_totale,
    rdt = winsorize(rdt_brut, probs = c(0.01, 0.99))
  ) %>%
  select(hhid, rdt)

# Jointure directe avec le climat pré-extrait — plus besoin de coordonnées GPS
# ni d'objet spatial ici, tout a déjà été extrait aux points en amont.
df_rdt <- base_rdt %>%
  inner_join(climat_menages, by = "hhid")

if (nrow(df_rdt) == 0) stop("Aucun ménage producteur avec climat extrait et rendement > 0 trouvé.")
cat("Ménages producteurs avec rendement et climat :", nrow(df_rdt), "\n")

# ------------------------------------------------------------------------------
# 3. Analyse exploratoire et cartographie
# ------------------------------------------------------------------------------

cat("--- Corrélations rendement / climat ---\n")
if (all(!is.na(df_rdt$pluie_2021))) {
  cor_pluie <- cor(df_rdt$rdt, df_rdt$pluie_2021, use = "complete.obs")
  cat("Corrélation rendement / pluviométrie 2021 :", round(cor_pluie, 3), "\n")
}
if (all(!is.na(df_rdt$pluie_saison_2021))) {
  cor_saison <- cor(df_rdt$rdt, df_rdt$pluie_saison_2021, use = "complete.obs")
  cat("Corrélation rendement / pluie saisonnière 2021 :", round(cor_saison, 3), "\n")
}
if ("temp_moy" %in% names(df_rdt) && all(!is.na(df_rdt$temp_moy))) {
  cor_temp <- cor(df_rdt$rdt, df_rdt$temp_moy, use = "complete.obs")
  cat("Corrélation rendement / température moyenne :", round(cor_temp, 3), "\n")
}

# Graphique de dispersion pluie vs rendement
p1 <- ggplot(df_rdt, aes(x = pluie_2021, y = rdt)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Précipitations annuelles 2021 (mm)", y = "Rendement mil winsorisé (kg/ha)",
       title = "Rendement du mil et pluviométrie")
ggsave(here::here("outputs", "figures", "rdt_vs_pluie.png"), p1, width = 8, height = 5)

# Carte des précipitations totales 2021 — raster déjà cadré/masqué au Mali,
# plus besoin de crop()/mask() ici.
carte_pluie <- tm_shape(chirps_mali) +
  tm_raster(title = "Précipitations (mm)", palette = "Blues", style = "cont") +
  tm_shape(mali_regions) + tm_borders(lwd = 0.5) +
  tm_layout(title = "Précipitations totales 2021 (CHIRPS)",
            legend.outside = TRUE)
tmap_save(carte_pluie, here::here("outputs", "maps", "precipitations_2021.png"),
          width = 8, height = 6)

# ------------------------------------------------------------------------------
# 4. Sauvegarde de la base enrichie
# ------------------------------------------------------------------------------

saveRDS(df_rdt, here::here("data", "processed", "base_mil_climat.rds"))

cat("\nAnalyse spatiale terminée. Base enrichie sauvegardée dans data/processed/base_mil_climat.rds\n")