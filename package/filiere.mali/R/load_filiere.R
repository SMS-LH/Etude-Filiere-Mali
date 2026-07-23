#' Charger et harmoniser un fichier de données d'enquête
#'
#' Charge un fichier de données (format Stata .dta par défaut) et harmonise les
#' identifiants du ménage (conversion en entier) pour sécuriser les jointures.
#' Peut optionnellement joindre un fichier de pondérations.
#'
#' @param chemin Chemin du fichier de données à charger.
#' @param id_menage Vecteur des colonnes identifiant le ménage, converties en
#'   entier (défaut c("grappe", "menage")).
#' @param ponderations Data frame optionnel de pondérations à joindre (doit
#'   contenir les colonnes de \code{id_menage}). Si NULL, pas de jointure.
#' @param lecteur Fonction de lecture du fichier (défaut haven::read_dta).
#'
#' @return Un data frame (tibble) avec les identifiants ménage harmonisés et,
#'   le cas échéant, les pondérations jointes.
#'
#' @export
load_filiere <- function(chemin,
                         id_menage = c("grappe", "menage"),
                         ponderations = NULL,
                         lecteur = haven::read_dta) {

  if (!file.exists(chemin)) {
    stop("Fichier introuvable : ", chemin)
  }
  data <- lecteur(chemin)

  for (id in id_menage) {
    if (id %in% names(data)) {
      data[[id]] <- as.integer(data[[id]])
    } else {
      warning("Colonne d'identifiant absente : ", id)
    }
  }

  if (!is.null(ponderations)) {
    for (id in id_menage) {
      if (id %in% names(ponderations)) {
        ponderations[[id]] <- as.integer(ponderations[[id]])
      }
    }
    data <- dplyr::left_join(data, ponderations, by = id_menage)
  }

  data
}
