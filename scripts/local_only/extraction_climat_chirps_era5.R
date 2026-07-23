# ==============================================================================
# scripts/local_only/extraction_climat_chirps_era5.R
# ==============================================================================

source(here::here("scripts", "00_setup.R"))

cat("Extraction climat (usage local uniquement, non soumis)\n")

# ------------------------------------------------------------------------------
# 1. Chargement des rasters bruts (lourds — jamais soumis)
# ------------------------------------------------------------------------------

chirps_files <- list.files(
  paths$raw_climat,
  pattern = "chirps-v2\\.0\\.\\d{4}\\.\\d{2}\\.tif$",
  full.names = TRUE
)
if (length(chirps_files) == 0) {
  stop("Aucun fichier CHIRPS trouvé dans ", paths$raw_climat,
       " — ce script nécessite les rasters bruts, en local uniquement.")
}

chirps_rast <- rast(chirps_files)
dates <- gsub(".*chirps-v2\\.0\\.(\\d{4})\\.(\\d{2})\\.tif$", "\\1-\\2", chirps_files)
names(chirps_rast) <- dates

era5_file <- list.files(paths$raw_climat, pattern = "era5_temp_mali_2021_2022\\.nc$",
                        full.names = TRUE)
temp_rast <- if (length(era5_file) > 0) rast(era5_file) else {
  warning("Fichier ERA5 non trouvé, la température ne sera pas extraite.")
  NULL
}

mali_regions <- st_read(here::here("data", "raw", "spatial", "gadm", "gadm41_MLI_1.shp"),
                        quiet = TRUE)

# ------------------------------------------------------------------------------
# 2. Points GPS des ménages producteurs de mil
# ------------------------------------------------------------------------------

s00_raw <- load_dta(paths$raw_menage, "s00_me_mli2021.dta") %>%
  mutate(hhid = paste(grappe, menage, vague, sep = "_"))

base_profil <- load_rds(paths$processed, "typologie_mil.rds")

base_rdt <- base_profil %>%
  filter(producteur_mil == 1, superficie_totale > 0, prod_totale_kg > 0) %>%
  mutate(
    rdt_brut = prod_totale_kg / superficie_totale,
    rdt = winsorize(rdt_brut, probs = c(0.01, 0.99))
  ) %>%
  select(hhid, rdt)

gps <- s00_raw %>%
  select(hhid, latitude = GPS__Latitude, longitude = GPS__Longitude) %>%
  filter(!is.na(latitude), !is.na(longitude))

base_rdt <- base_rdt %>%
  left_join(gps, by = "hhid") %>%
  filter(!is.na(latitude))

if (nrow(base_rdt) == 0) stop("Aucun ménage producteur avec coordonnées GPS et rendement > 0 trouvé.")
cat("Ménages producteurs avec rendement et GPS :", nrow(base_rdt), "\n")

points_sf <- st_as_sf(base_rdt, coords = c("longitude", "latitude"), crs = 4326)

# ------------------------------------------------------------------------------
# 3. Extraction des valeurs climatiques aux points GPS (seule étape lourde)
# ------------------------------------------------------------------------------

# 3.1 Précipitations CHIRPS
extr_chirps <- terra::extract(chirps_rast, vect(points_sf), ID = FALSE)

dates_chirps <- names(extr_chirps)
annees_mois <- strsplit(dates_chirps, "-")
annees <- sapply(annees_mois, `[`, 1)
mois   <- sapply(annees_mois, `[`, 2)

pluie_2021        <- rowSums(extr_chirps[, annees == "2021", drop = FALSE], na.rm = TRUE)
pluie_2022        <- rowSums(extr_chirps[, annees == "2022", drop = FALSE], na.rm = TRUE)
pluie_saison_2021 <- rowSums(extr_chirps[, annees == "2021" & mois %in% c("06","07","08","09"), drop = FALSE], na.rm = TRUE)

# 3.2 Température ERA5
temp_moy <- if (!is.null(temp_rast)) {
  extr_temp <- terra::extract(temp_rast, vect(points_sf), ID = FALSE)
  rowMeans(extr_temp, na.rm = TRUE)
} else {
  NA_real_
}

# ------------------------------------------------------------------------------
# 4. Sauvegarde du fichier léger n°1 : climat par ménage (quelques Ko)
# ------------------------------------------------------------------------------

climat_menages <- base_rdt %>%
  select(hhid) %>%
  mutate(
    pluie_2021        = pluie_2021,
    pluie_2022        = pluie_2022,
    pluie_saison_2021 = pluie_saison_2021,
    temp_moy          = temp_moy
  )

saveRDS(climat_menages, here::here("data", "raw", "climat", "climat_menages_mil.rds"))
cat("Sauvegardé : data/raw/climat/climat_menages_mil.rds (", nrow(climat_menages), "ménages )\n")

# ------------------------------------------------------------------------------
# 5. Sauvegarde du fichier léger n°2 : un seul raster cadré Mali (pour la carte)
# ------------------------------------------------------------------------------
# Un unique raster (précipitations totales 2021, cadré/masqué au Mali) au lieu
# de la pile complète des .tif mensuels bruts — quelques centaines de Ko à
# quelques Mo au lieu de plusieurs Go.

chirps_2021_total <- sum(chirps_rast[[grep("2021", names(chirps_rast))]])
mali_vect <- vect(mali_regions)
chirps_mali <- crop(chirps_2021_total, mali_vect) %>% mask(mali_vect)

writeRaster(
  chirps_mali,
  here::here("data", "raw", "climat", "precip_mali_2021_total.tif"),
  overwrite = TRUE
)
cat("Sauvegardé : data/raw/climat/precip_mali_2021_total.tif\n")

cat("\nExtraction terminée. Tu peux maintenant ignorer/supprimer les fichiers\n")
cat("CHIRPS/.tif mensuels bruts et le .nc ERA5 pour la soumission — seuls les\n")
cat("deux fichiers ci-dessus sont nécessaires à 09_analyse_spatiale.R.\n")