#' Calculer ou traiter un prix unitaire le long de la chaîne de valeur
#'
#' Deux modes : (1) reconstitution du prix à partir d'un montant et d'une
#' quantité (\code{col_montant} et \code{col_quantite}), utile quand seules les
#' transactions sont connues ; (2) prix déjà relevé fourni directement via
#' \code{col_prix}. Dans les deux cas, un nettoyage des aberrations et une
#' agrégation pondérée par strate peuvent être appliqués.
#'
#' @param data Un data frame.
#' @param col_prix Nom d'une colonne de prix déjà disponible. Si fourni, le
#'   prix est utilisé tel quel (mode "prix direct").
#' @param col_montant,col_quantite Noms des colonnes de montant et de quantité,
#'   utilisés pour reconstituer le prix si \code{col_prix} est NULL.
#' @param methode_nettoyage "iqr" (défaut), "bornes" ou "aucun".
#' @param iqr_facteur Multiple de l'écart interquartile (défaut 3).
#' @param prix_min,prix_max Bornes absolues (si methode = "bornes").
#' @param strata Nom(s) de colonne(s) d'agrégation. Si NULL, prix par ligne.
#' @param weights Colonne de poids pour l'agrégation. Si NULL, moyenne simple.
#'
#' @return Si \code{strata} est NULL, le data frame avec \code{prix_unitaire}.
#'   Sinon, un data frame agrégé par strate (\code{prix_moyen}, \code{n}).
#'
#' @export
prix_chaine <- function(data,
                        col_prix = NULL,
                        col_montant = NULL,
                        col_quantite = NULL,
                        methode_nettoyage = c("iqr", "bornes", "aucun"),
                        iqr_facteur = 3,
                        prix_min = NULL,
                        prix_max = NULL,
                        strata = NULL,
                        weights = NULL) {

  methode_nettoyage <- match.arg(methode_nettoyage)

  if (!is.null(col_prix)) {
    prix <- as.numeric(data[[col_prix]])
  } else {
    if (is.null(col_montant) || is.null(col_quantite)) {
      stop("Fournir soit col_prix, soit col_montant ET col_quantite.")
    }
    prix <- as.numeric(data[[col_montant]]) / as.numeric(data[[col_quantite]])
  }
  prix[!is.finite(prix)] <- NA

  if (methode_nettoyage == "iqr") {
    q <- stats::quantile(prix, c(0.25, 0.75), na.rm = TRUE)
    iqr <- q[2] - q[1]
    prix[prix < q[1] - iqr_facteur*iqr | prix > q[2] + iqr_facteur*iqr] <- NA
  } else if (methode_nettoyage == "bornes") {
    if (!is.null(prix_min)) prix[prix < prix_min] <- NA
    if (!is.null(prix_max)) prix[prix > prix_max] <- NA
  }

  data$prix_unitaire <- prix

  if (is.null(strata)) return(data)

  data_ok <- data[!is.na(data$prix_unitaire), , drop = FALSE]
  cle <- interaction(data_ok[strata], drop = TRUE)
  resultats <- lapply(split(data_ok, cle), function(bloc) {
    if (!is.null(weights)) {
      prix_moy <- stats::weighted.mean(bloc$prix_unitaire,
                                       as.numeric(bloc[[weights]]), na.rm = TRUE)
    } else {
      prix_moy <- mean(bloc$prix_unitaire, na.rm = TRUE)
    }
    cbind(bloc[1, strata, drop = FALSE],
          data.frame(prix_moyen = prix_moy, n = nrow(bloc)))
  })
  do.call(rbind, resultats)
}
