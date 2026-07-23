#' Calculer le rendement agricole avec plafonnement et winsorisation
#'
#' Calcule le rendement (production / superficie) au niveau ménage pour une
#' culture donnée, applique un plafond physiologique issu de la table
#' \code{plafonds_rendement} et winsorise les valeurs par strate.
#'
#' @param data Un data frame au niveau ménage contenant la production et la
#'   superficie.
#' @param culture Nom (anglais) de la culture présent dans la colonne
#'   \code{culture} de \code{plafonds_rendement} (ex. "millet", "maize").
#' @param col_production Nom de la colonne production en kg. Défaut
#'   "production_kg".
#' @param col_superficie Nom de la colonne superficie en ha. Défaut
#'   "superficie_ha".
#' @param col_prod_hors_stock Nom (optionnel) d'une colonne de production hors
#'   stock. Si fourni, les ménages dépassant déjà le plafond avec cette
#'   quantité sont exclus.
#' @param strata Vecteur de noms de colonnes de stratification pour la
#'   winsorisation. Si NULL, pas de winsorisation.
#' @param p_low,p_high Percentiles de winsorisation (défaut 0.01 et 0.99).
#' @param table_plafonds Table des plafonds. Par défaut celle du package.
#'
#' @return Le data frame d'entrée, avec les colonnes ajoutées :
#'   \code{rendement_brut}, \code{rendement_plafonne} et, si \code{strata}
#'   est fourni, \code{rendement_wins}.
#'
#' @export
calc_rendement <- function(data,
                           culture,
                           col_production = "production_kg",
                           col_superficie = "superficie_ha",
                           col_prod_hors_stock = NULL,
                           strata = NULL,
                           p_low = 0.01,
                           p_high = 0.99,
                           table_plafonds = filiere.mali::plafonds_rendement) {

  ligne_plafond <- table_plafonds[table_plafonds$culture == culture, ]
  if (nrow(ligne_plafond) == 0) {
    stop("Culture '", culture, "' absente de la table des plafonds.")
  }
  plafond <- ligne_plafond$plafond_kg_ha[1]

  prod <- data[[col_production]]
  sup  <- data[[col_superficie]]
  data$rendement_brut <- prod / sup

  if (!is.null(col_prod_hors_stock)) {
    rdt_hors_stock <- data[[col_prod_hors_stock]] / sup
    data <- data[rdt_hors_stock <= plafond | is.na(rdt_hors_stock), ]
    prod <- data[[col_production]]
    sup  <- data[[col_superficie]]
  }

  prod_plafonnee <- pmin(prod, plafond * sup)
  data$rendement_plafonne <- prod_plafonnee / sup

  if (!is.null(strata)) {
    cle <- interaction(data[strata], drop = TRUE)
    data$rendement_wins <- data$rendement_plafonne
    for (s in levels(cle)) {
      idx <- which(cle == s)
      vals <- data$rendement_plafonne[idx]
      bornes <- quantile(vals, c(p_low, p_high), na.rm = TRUE)
      data$rendement_wins[idx] <- pmin(pmax(vals, bornes[1]), bornes[2])
    }
  }

  data
}
