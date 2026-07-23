# ==============================================================================
# run_all.R
# Script maître : exécute l'ensemble des analyses dans l'ordre.

# ==============================================================================

# Charger la configuration
source(here::here("scripts", "00_setup.R"))

skip_etape3 <- TRUE   # Mettre FALSE pour inclure l'EDA transformation

# ------------------------------------------------------------------------------
# 01 – Import et assemblage des données brutes
# ------------------------------------------------------------------------------
message(">>> 01_import.R")
source(here::here("scripts", "01_import.R"))

# ------------------------------------------------------------------------------
# 02 – Étape 0 : Choix du produit
# ------------------------------------------------------------------------------
message(">>> 02_etape0_choix_produit.R")
source(here::here("scripts", "02_etape0_choix_produit.R"))

# ------------------------------------------------------------------------------
# 03 – Étape 1 : Production et rendements du mil
# ------------------------------------------------------------------------------
message(">>> 03_etape1_production.R")
source(here::here("scripts", "03_etape1_production.R"))

# ------------------------------------------------------------------------------
# 04 – Étape 2 : Commercialisation et prix
# ------------------------------------------------------------------------------
message(">>> 04_etape2_commercialisation.R")
source(here::here("scripts", "04_etape2_commercialisation.R"))

# ------------------------------------------------------------------------------
# 05 – Étape 3 : Transformation
# ------------------------------------------------------------------------------
if (!skip_etape3) {
  message(">>> 05_etape3_transformation.R")
  source(here::here("scripts", "05_etape3_transformation.R"))
} else {
  message(">>> Étape 3 sautée (transformation marginale)")
}

# ------------------------------------------------------------------------------
# 06 – Étape 4 : Consommation et demande
# ------------------------------------------------------------------------------
message(">>> 06_etape4_consommation.R")
source(here::here("scripts", "06_etape4_consommation.R"))

# ------------------------------------------------------------------------------
# 07 – Étape 5 : Profilage intégré des ménages
# ------------------------------------------------------------------------------
message(">>> 07_etape5_profilage.R")
source(here::here("scripts", "07_etape5_profilage.R"))

# ------------------------------------------------------------------------------
# 08 – Étape 6 : Analyse d'impact sur la sécurité alimentaire
# ------------------------------------------------------------------------------
message(">>> 08_etape6_impact.R")
source(here::here("scripts", "08_etape6_impact.R"))

# ------------------------------------------------------------------------------
# 09 – Analyse spatiale et climatique
# ------------------------------------------------------------------------------
message(">>> 09_analyse_spatiale.R")
source(here::here("scripts", "09_analyse_spatiale.R"))

# ------------------------------------------------------------------------------
# 10 – Export des données pour le dashboard Shiny
# ------------------------------------------------------------------------------
message(">>> 10_export_shiny.R")
source(here::here("scripts", "10_export_shiny.R"))

message("=== run_all terminé avec succès. ===")