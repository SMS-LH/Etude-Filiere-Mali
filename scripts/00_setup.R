# ==============================================================================
# 00_setup.R
# Configuration initiale du projet – Packages, chemins, options, fonctions
# utilitaires, thème graphique et exports Excel.
# À exécuter en premier : source(here::here("scripts", "00_setup.R"))
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------------------------

packages_necessaires <- c(
  "here",         # Chemins relatifs portables
  "haven",        # Lecture de fichiers Stata (.dta)
  "readxl",       # Lecture de fichiers Excel
  "openxlsx",     # Création de classeurs Excel
  "readr",        # Lecture de CSV (FAOSTAT)                    
  "dplyr",        # Transformation de données
  "tidyr",        # Remise en forme des données
  "stringr",      # Manipulation de chaînes
  "lubridate",    # Gestion des dates
  "forcats",      # Gestion des facteurs
  "ggplot2",      # Graphiques
  "scales",       # Formats d'axes
  "patchwork",    # Assemblage de graphiques
  "sf",           # Données vectorielles spatiales
  "terra",        # Données raster (climat)
  "tmap",         # Cartographie thématique
  "leaflet",      # Cartes interactives
  "fixest",       # Régressions avec effets fixes
  "estimatr",     # Erreurs robustes
  "modelsummary", # Tableaux de résultats de modèles
  "WDI",          # Indicateurs Banque Mondiale
  "shiny",        # Dashboard interactif
  "bslib",        # Thème Bootstrap pour Shiny
  "bsicons",      # Icônes Bootstrap pour Shiny
  "plotly",        # Graphiques interactifs
  "shinyjs",      # Fonctions JavaScript pour Shiny
  "httr"          # Gestion des requêtes HTTP pour Shiny
)

for (pkg in packages_necessaires) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
invisible(lapply(packages_necessaires, library, character.only = TRUE))

message("00_setup.R – Packages OK : ", length(packages_necessaires), " packages chargés")

# ------------------------------------------------------------------------------
# 2. Options globales
# ------------------------------------------------------------------------------

options(
  stringsAsFactors = FALSE,
  scipen           = 999,
  timeout          = 300,
  encoding         = "UTF-8"
)
set.seed(2021)

message("00_setup.R – Options globales configurées")

# ------------------------------------------------------------------------------
# 3. Chemins du projet
# ------------------------------------------------------------------------------

racine <- here::here()

paths <- list(
  raw_menage      = file.path(racine, "data", "raw", "EHCVM", "menage"),
  raw_communaute  = file.path(racine, "data", "raw", "EHCVM", "communaute"),
  raw_auxiliaires = file.path(racine, "data", "raw", "EHCVM", "auxiliaires"),
  raw_spatial     = file.path(racine, "data", "raw", "spatial"),
  raw_climat      = file.path(racine, "data", "raw", "climat"),
  raw_faostat     = file.path(racine, "data", "raw", "faostat"),
  raw_wdi         = file.path(racine, "data", "raw", "wdi"),
  raw_reference   = file.path(racine, "data", "raw", "reference"),
  processed       = file.path(racine, "data", "processed"),
  figures         = file.path(racine, "outputs", "figures"),
  tables          = file.path(racine, "outputs", "tables"),
  maps            = file.path(racine, "outputs", "maps"),
  scripts         = file.path(racine, "scripts"),
  package         = file.path(racine, "package"),
  shiny           = file.path(racine, "shiny"),
  docs            = file.path(racine, "docs")
)

message("00_setup.R – Chemins configurés")

# ------------------------------------------------------------------------------
# 4. Fonctions utilitaires générales
# ------------------------------------------------------------------------------

create_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}

load_dta <- function(folder, filename) {
  full_path <- file.path(folder, filename)
  if (!file.exists(full_path)) stop("Fichier introuvable : ", full_path)
  haven::read_dta(full_path)
}

charger_dta <- function(prefix) {
  filename <- paste0(prefix, "_me_mli2021.dta")
  load_dta(paths$raw_menage, filename)
}

save_rds <- function(object, folder, filename) {
  create_dir(folder)
  full_path <- file.path(folder, filename)
  saveRDS(object, full_path)
  message("Sauvegarde : ", full_path)
}

load_rds <- function(folder, filename) {
  full_path <- file.path(folder, filename)
  if (!file.exists(full_path)) stop("Fichier introuvable : ", full_path)
  readRDS(full_path)
}

check_file <- function(file_path) {
  if (!file.exists(file_path)) {
    warning("Fichier manquant : ", file_path)
    return(FALSE)
  }
  TRUE
}

winsorize <- function(x, probs = c(0.01, 0.99)) {
  q <- quantile(x, probs = probs, na.rm = TRUE)
  x[x < q[1]] <- q[1]
  x[x > q[2]] <- q[2]
  x
}

safe_divide <- function(num, den) {
  ifelse(den != 0, num / den, NA_real_)
}

# ------------------------------------------------------------------------------
# 5. Fonctions pour classeurs Excel bien mis en forme
# ------------------------------------------------------------------------------

#' Créer un classeur Excel avec un style prédéfini
#'
#' @param data Data frame à exporter
#' @param sheet_name Nom de la feuille
#' @param title Titre optionnel à placer en haut de la feuille
#' @param filename Nom du fichier xlsx (sera sauvegardé dans paths$tables)
#'
#' @return Un objet workbook (invisible) ; sauvegarde également le fichier.
export_excel <- function(data, sheet_name, title = NULL, filename) {
  wb <- createWorkbook()
  addWorksheet(wb, sheet_name)
  
  header_style <- createStyle(fontColour = "#ffffff", fgFill = "#4B5320",
                              halign = "center", textDecoration = "bold",
                              border = "TopBottomLeftRight")
  title_style <- createStyle(fontSize = 14, fontColour = "#2E4053", textDecoration = "bold")
  body_style <- createStyle(halign = "left", border = "TopBottomLeftRight")
  
  if (!is.null(title)) {
    writeData(wb, sheet_name, title, startCol = 1, startRow = 1)
    mergeCells(wb, sheet_name, cols = 1:ncol(data), rows = 1)
    addStyle(wb, sheet_name, title_style, rows = 1, cols = 1:ncol(data), gridExpand = TRUE)
    start_row <- 3
  } else {
    start_row <- 1
  }
  
  writeData(wb, sheet_name, data, startRow = start_row)
  addStyle(wb, sheet_name, header_style, rows = start_row, cols = 1:ncol(data), gridExpand = TRUE)
  addStyle(wb, sheet_name, body_style, rows = (start_row+1):(start_row+nrow(data)),
           cols = 1:ncol(data), gridExpand = TRUE)
  
  setColWidths(wb, sheet_name, cols = 1:ncol(data), widths = "auto")
  
  full_path <- file.path(paths$tables, filename)
  saveWorkbook(wb, full_path, overwrite = TRUE)
  message("Classeur Excel sauvegardé : ", full_path)
  invisible(wb)
}

# ------------------------------------------------------------------------------
# 6. Thème graphique
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

theme_set(theme_filiere)

message("00_setup.R – Thème graphique appliqué")

# ------------------------------------------------------------------------------
# 7. Création des dossiers de sortie
# ------------------------------------------------------------------------------

dossiers_sortie <- c(
  paths$processed,
  paths$figures,
  paths$tables,
  paths$maps,
  paths$docs
)
for (d in dossiers_sortie) create_dir(d)

message("00_setup.R – Dossiers de sortie vérifiés")

# ------------------------------------------------------------------------------
# 8. Vérification des fichiers de données brutes
# ------------------------------------------------------------------------------

fichiers_menage <- c(
  "s00_me_mli2021.dta", "s01_me_mli2021.dta", "s07b_me_mli2021.dta",
  "s08a_me_mli2021.dta", "s16a_me_mli2021.dta", "s16c_me_mli2021.dta",
  "s16d_me_mli2021.dta"
)
fichiers_comm <- c(
  "s00_co_mli2021.dta", "s01_co_mli2021.dta",
  "s02_co_mli2021.dta", "s03_co_mli2021.dta"
)
fichiers_aux <- c(
  "ehcvm_prix_mli2021.dta", "ehcvm_nsu_mli2021.dta",
  "ehcvm_ponderations_mli2021.dta", "calorie_conversion_wa_2021.dta"
)

sapply(file.path(paths$raw_menage, fichiers_menage), check_file)
sapply(file.path(paths$raw_communaute, fichiers_comm), check_file)
sapply(file.path(paths$raw_auxiliaires, fichiers_aux), check_file)

check_file(file.path(paths$raw_reference, "table_conversion_4unites.xlsx"))

message("00_setup.R – Vérification des fichiers bruts terminée")

# ------------------------------------------------------------------------------
# 9. Message de confirmation finale
# ------------------------------------------------------------------------------

message("============================================================")
message(" Projet : Étude de filière – EHCVM Mali 2021/2022")
message(" Packages chargés : ", length(packages_necessaires))
message(" Dossier racine   : ", racine)
message("============================================================")
message("00_setup.R – Prêt à l'emploi")