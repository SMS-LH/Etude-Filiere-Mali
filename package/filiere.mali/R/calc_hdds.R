#' Calculer le score de diversité alimentaire des ménages (HDDS)
#'
#' Compte le nombre de groupes alimentaires distincts consommés par chaque
#' ménage, à partir de données de consommation en format long et d'une
#' correspondance produit → groupe alimentaire.
#'
#' @param data Un data frame de consommation en format long, avec une colonne
#'   d'identifiant ménage, une colonne de produit, et une indication de
#'   consommation.
#' @param col_menage Nom(s) de colonne(s) identifiant le ménage
#'   (ex. c("grappe", "menage")).
#' @param col_produit Nom de la colonne du code produit.
#' @param correspondance Un data frame à deux colonnes : le code produit et le
#'   groupe alimentaire correspondant.
#' @param col_corr_produit Nom de la colonne "produit" dans
#'   \code{correspondance} (défaut "produit").
#' @param col_corr_groupe Nom de la colonne "groupe" dans
#'   \code{correspondance} (défaut "groupe").
#' @param col_consomme Nom (optionnel) d'une colonne indiquant si le produit a
#'   été consommé. Si NULL, toutes les lignes sont considérées comme
#'   consommées.
#' @param code_consomme Valeur indiquant la consommation (défaut 1).
#'
#' @return Un data frame au niveau ménage avec la colonne \code{hdds}.
#'
#' @export
calc_hdds <- function(data,
                      col_menage,
                      col_produit,
                      correspondance,
                      col_corr_produit = "produit",
                      col_corr_groupe = "groupe",
                      col_consomme = NULL,
                      code_consomme = 1) {

  if (!is.null(col_consomme)) {
    garde <- as.integer(data[[col_consomme]]) == code_consomme
    data <- data[garde & !is.na(garde), , drop = FALSE]
  }

  corr <- correspondance[, c(col_corr_produit, col_corr_groupe)]
  names(corr) <- c("._produit", "._groupe")
  data$._produit <- data[[col_produit]]
  data <- merge(data, corr, by = "._produit", all.x = TRUE)

  data <- data[!is.na(data$._groupe), , drop = FALSE]

  cle <- interaction(data[col_menage], drop = TRUE)
  groupes_par_menage <- tapply(data$._groupe, cle,
                               function(g) length(unique(g)))

  menages <- unique(data[c(col_menage)])
  menages$._cle <- interaction(menages[col_menage], drop = TRUE)
  menages$hdds <- as.integer(groupes_par_menage[as.character(menages$._cle)])
  menages$._cle <- NULL

  menages
}
