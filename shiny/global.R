# shiny/global.R
library(shiny)
library(bslib)
library(bsicons)
library(plotly)
library(leaflet)
library(dplyr)
library(sf)
library(ggplot2)
library(here)

# --- chemins via here ---
chemin_data <- here::here("shiny", "data", "dashboard_data.rds")
dash <- readRDS(chemin_data)

chemin_gadm <- here::here("data", "raw", "spatial", "gadm", "gadm41_MLI_1.shp")
mali_regions <- sf::st_read(chemin_gadm, quiet = TRUE)

corresp_region <- data.frame(
  region = 1:9,
  NAME_1 = c("Kayes","Koulikoro","Sikasso","Ségou","Mopti",
             "Timbuktu","Gao","Kidal","Bamako"),
  stringsAsFactors = FALSE
)

couleur_accent <- "#ef4444"
couleur_mil    <- "#d4a017"

# --- Contexte pour l'agent conversationnel ---
chemin_contexte <- here::here("shiny", "data", "context_llm.txt")
if (file.exists(chemin_contexte)) {
  contexte_chat <- paste(readLines(chemin_contexte, warn = FALSE), collapse = "\n")
} else {
  contexte_chat <- "Tu es un assistant spécialiste de la filière mil au Mali."
}

# --- Clés API (Groq en priorité, Gemini en repli) ---
groq_api_key <- Sys.getenv("GROQ_API_KEY")
if (groq_api_key == "") {
  message("GROQ_API_KEY non trouvée. L'agent IA tentera Gemini, puis Ollama, puis un mode dégradé.")
}

gemini_api_key <- Sys.getenv("GEMINI_API_KEY")
if (gemini_api_key == "") {
  message("GEMINI_API_KEY non trouvée. En cas d'échec de Groq, l'agent IA tentera Ollama, puis un mode dégradé.")
}

# --- Fonctions utilitaires pour le LLM ---
source(here::here("shiny", "R", "utils_llm.R"))