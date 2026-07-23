#' Régression de filière avec effets fixes, poids et erreurs groupées
#'
#' Estime par MCO (via \code{fixest::feols}) l'effet de variables de filière
#' sur un résultat, avec contrôles, effets fixes, poids d'enquête et erreurs
#' groupées optionnels. Sert aussi bien aux rendements qu'aux scores de
#' sécurité alimentaire (FIES, HDDS).
#'
#' @param data Un data frame au niveau ménage.
#' @param outcome Nom (chaîne) de la variable à expliquer (ex. "ln_rendement").
#' @param filiere_vars Vecteur de noms des variables d'intérêt
#'   (ex. c("ln_intrants", "fertilite")).
#' @param controls Vecteur de noms des variables de contrôle (défaut NULL).
#' @param fixed_effects Vecteur de noms des effets fixes (ex. "grappe"),
#'   ou NULL pour aucun.
#' @param weights Nom de la variable de poids (ex. "hhweight"), ou NULL.
#' @param cluster Nom de la variable de clustering des erreurs (ex. "grappe"),
#'   ou NULL.
#'
#' @return Un objet \code{fixest} (résultat de l'estimation).
#'
#' @export
reg_filiere <- function(data,
                        outcome,
                        filiere_vars,
                        controls = NULL,
                        fixed_effects = NULL,
                        weights = NULL,
                        cluster = NULL) {

  rhs <- paste(c(filiere_vars, controls), collapse = " + ")

  if (!is.null(fixed_effects)) {
    fe <- paste(fixed_effects, collapse = " + ")
    formule <- stats::as.formula(paste(outcome, "~", rhs, "|", fe))
  } else {
    formule <- stats::as.formula(paste(outcome, "~", rhs))
  }

  w <- if (!is.null(weights)) stats::as.formula(paste("~", weights)) else NULL
  cl <- if (!is.null(cluster)) stats::as.formula(paste("~", cluster)) else NULL

  fixest::feols(formule, data = data, weights = w, cluster = cl)
}
