#' Calculer le score FIES de sécurité alimentaire
#'
#' Compte les réponses affirmatives à une série de questions FIES (échelle
#' d'expérience de l'insécurité alimentaire de la FAO) et classe les ménages
#' en insécurité modérée et sévère selon des seuils.
#'
#' @param data Un data frame au niveau ménage contenant les questions FIES.
#' @param questions Vecteur des noms de colonnes des questions FIES (dans
#'   l'ordre de sévérité). Le standard FAO en compte 8.
#' @param code_oui Valeur codant la réponse affirmative (défaut 1).
#' @param seuil_modere Score à partir duquel l'insécurité est modérée ou plus
#'   (défaut 4).
#' @param seuil_severe Score à partir duquel l'insécurité est sévère
#'   (défaut 7).
#'
#' @return Le data frame d'entrée avec les colonnes ajoutées :
#'   \code{score_fies} (0 à n questions), \code{insecurite_moderee} et
#'   \code{insecurite_severe} (indicatrices 0/1).
#'
#' @export
calc_fies <- function(data,
                      questions,
                      code_oui = 1,
                      seuil_modere = 4,
                      seuil_severe = 7) {

  manquantes <- setdiff(questions, names(data))
  if (length(manquantes) > 0) {
    stop("Questions FIES absentes du data frame : ",
         paste(manquantes, collapse = ", "))
  }

  # Extraire les colonnes et convertir en matrice numérique
  mat <- as.matrix(data[questions])
  # Convertir en 0/1 (1 si la valeur == code_oui)
  mat01 <- ifelse(mat == code_oui, 1L, 0L)
  # Reformer en matrice pour garantir le bon traitement même avec 1 ligne
  dim(mat01) <- dim(mat)

  data$score_fies <- rowSums(mat01, na.rm = TRUE)
  data$insecurite_moderee <- as.integer(data$score_fies >= seuil_modere)
  data$insecurite_severe  <- as.integer(data$score_fies >= seuil_severe)

  data
}
