#' Profiler des groupes de ménages
#'
#' Calcule, pour chaque groupe de ménages, les moyennes pondérées d'un ensemble
#' de variables numériques et les parts pondérées de variables binaires. Sert
#' à caractériser une typologie (ex. les groupes d'une filière).
#'
#' @param data Un data frame au niveau ménage.
#' @param groupe Nom de la colonne définissant les groupes.
#' @param vars_moyennes Vecteur de noms de variables numériques dont on veut la
#'   moyenne pondérée (ex. age, taille, niveau de vie).
#' @param vars_parts Vecteur de noms de variables binaires (0/1) dont on veut
#'   la part pondérée en pourcentage (ex. rural, alphabétisé). Optionnel.
#' @param poids Nom de la colonne de poids (défaut NULL = non pondéré).
#'
#' @return Un data frame avec une ligne par groupe : effectif, moyennes des
#'   \code{vars_moyennes}, et parts (en %) des \code{vars_parts}.
#'
#' @export
profil_menage <- function(data,
                          groupe,
                          vars_moyennes,
                          vars_parts = NULL,
                          poids = NULL) {

  w <- if (!is.null(poids)) as.numeric(data[[poids]]) else rep(1, nrow(data))

  cle <- data[[groupe]]
  blocs <- split(seq_len(nrow(data)), cle)

  lignes <- lapply(names(blocs), function(g) {
    idx <- blocs[[g]]
    wi <- w[idx]

    res <- data.frame(groupe = g, n = length(idx))

    for (v in vars_moyennes) {
      res[[v]] <- stats::weighted.mean(as.numeric(data[[v]][idx]), wi,
                                       na.rm = TRUE)
    }
    if (!is.null(vars_parts)) {
      for (v in vars_parts) {
        res[[paste0("part_", v)]] <-
          100 * stats::weighted.mean(as.numeric(data[[v]][idx]), wi,
                                     na.rm = TRUE)
      }
    }
    res
  })

  resultat <- do.call(rbind, lignes)
  names(resultat)[1] <- groupe
  resultat
}
